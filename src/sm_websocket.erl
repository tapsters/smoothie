-module(sm_websocket).
-author("Vitaly Shutko").
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-type handler_state() :: any().
-type cowboy_req() :: tuple().
-type state() :: {handler_state(), cowboy_req()}.
-type data() :: any().
-type message() :: handshake
  | init
  | {stream, data()}
  | {info, any()}
  | terminate.

-callback handle(message(), state())
	-> any().

-record(state, {
  handler,
  protocol,
  handler_state
}).

%% HTTP

init(_Transport, Req, Opts) ->
  {handler, Handler} = proplists:lookup(handler, Opts),

  case cowboy_req:header(<<"upgrade">>, Req) of
    {undefined, Req2} ->
      {shutdown, Req2, undefined};
    {Bin, Req2} when is_binary(Bin) ->
      case cowboy_bstr:to_lower(Bin) of
        <<"websocket">> ->
          case Handler:handle(handshake, {undefined, Req}) of
            ok ->
              {upgrade, protocol, cowboy_websocket, Req2, Opts++[{handler_state, {}}]};
            {ok, HandlerState} ->
              {upgrade, protocol, cowboy_websocket, Req2, Opts++[{handler_state, HandlerState}]};
            shutdown ->
              {ok, Req3} = cowboy_req:reply(500, [], [], Req2),
              {shutdown, Req3, undefined};
            {shutdown, StatusCode} ->
              {ok, Req3} = cowboy_req:reply(StatusCode, [], [], Req2),
              {shutdown, Req3, undefined}
          end;
        _ ->
          {ok, Req3} = cowboy_req:reply(501, [], [], Req2),
          {shutdown, Req3, undefined}
      end;
    {_, Req2} -> {shutdown, Req2, undefined}
  end.

handle(Req, State) ->
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% WebSocket

websocket_init(_Transport, Req, Opts) ->
  {handler,       Handler}      = proplists:lookup(handler, Opts),
  {timeout,       Timeout}      = proplists:lookup(timeout, Opts),
  {protocol,      Protocol}     = proplists:lookup(protocol, Opts),
  {handler_state, HandlerState} = proplists:lookup(handler_state, Opts),

  State = #state{handler=Handler, protocol=Protocol},

  case Handler:handle(init, {HandlerState, Req}) of
    ok ->
      {ok, Req, State#state{handler_state=undefined}, Timeout, hibernate};
    {ok, HandlerState1} ->
      {ok, Req, State#state{handler_state=HandlerState1}, Timeout, hibernate};
    shutdown ->
      {shutdown, Req};
    {shutdown, _HandlerState} ->
      {shutdown, Req}
  end.

websocket_handle({text, <<"ping">>}, Req, State) ->
  {reply, {text, <<"pong">>}, Req, State, hibernate};
websocket_handle({Format, Data}, Req, State=#state{handler=Handler,
                                                   handler_state=HandlerState,
                                                   protocol=Protocol}) ->
  case Protocol:supports_format(Format) of
    true ->
      Decoded = Protocol:decode(Data, Format),
      case Handler:handle({stream, {Format, Decoded}}, {HandlerState, Req}) of
        ok ->
          {ok, Req, State, hibernate};
        {ok, HandlerState2} ->
          {ok, Req, State#state{handler_state=HandlerState2}, hibernate};
        {reply, Reply} ->
          Encoded = Protocol:encode(Reply, Format),
          {reply, {Format, Encoded}, Req, State, hibernate};
        {reply, Reply, HandlerState2} ->
          Encoded = Protocol:encode(Reply, Format),
          {reply, {Format, Encoded}, Req, State#state{handler_state=HandlerState2}, hibernate};
        shutdown ->
          {shutdown, Req, State};
        {shutdown, HandlerState2} ->
          {shutdown, Req, State#state{handler_state=HandlerState2}}
      end;
    false ->
      io:format("Protocol ~p doesn't support ~p format~n", [Protocol, Format]),
      {ok, Req, State, hibernate}
  end.

websocket_info(Info, Req, State=#state{handler=Handler, 
                                       handler_state=HandlerState,
                                       protocol=Protocol}) ->
  case Handler:handle({info, Info}, {HandlerState, Req}) of
    ok ->
      {ok, Req, State, hibernate};
    {ok, HandlerState2} ->
      {ok, Req, State#state{handler_state=HandlerState2}, hibernate};
    {reply, {Format, Data}} ->
      Encoded = Protocol:encode(Data, Format),
      {reply, {Format, Encoded}, Req, State, hibernate};
    {reply, {Format, Data}, HandlerState2} ->
      Encoded = Protocol:encode(Data, Format),
      {reply, {Format, Encoded}, Req, State#state{handler_state=HandlerState2}, hibernate};
    shutdown ->
      {shutdown, Req, State};
    {shutdown, HandlerState2} ->
      {shutdown, Req, State#state{handler_state=HandlerState2}}
end.

websocket_terminate(_Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
  Handler:handle(terminate, {HandlerState, Req}),
  ok.
