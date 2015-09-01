-module(sm_websocket).
-author("Vitaly Shutko").
-behaviour(cowboy_websocket).

-export([init/2, websocket_handle/3, websocket_info/3, terminate/3]).

-type handler_state() :: any().
-type cowboy_req() :: tuple().
-type state() :: {handler_state(), cowboy_req()}.
-type data() :: any().
-type message() :: handshake
  | init
  | {stream, data()}
  | {info, any()}
  | terminate.

-callback handle(message(), state()) -> any().

-record(state, {
  handler,
  protocol,
  handler_state
}).

%% Cowboy WebSocket Callbacks

init(Req, Opts) ->
  {handler,  Handler}  = proplists:lookup(handler, Opts),
  {protocol, Protocol} = proplists:lookup(protocol, Opts),
  {timeout,  Timeout}  = proplists:lookup(timeout, Opts),
  
  State = #state{handler=Handler, protocol=Protocol, handler_state={}},

  case Handler:handle(init, {{}, Req}) of
    ok ->
      {cowboy_websocket, Req, State, Timeout, hibernate};
    {ok, HandlerState} ->
      {cowboy_websocket, Req, State#state{handler_state=HandlerState}, Timeout, hibernate};
    shutdown ->
      {ok, cowboy_req:reply(500, Req), State};
    {shutdown, StatusCode} ->
      {ok, cowboy_req:reply(StatusCode, Req), State}
  end.

%% WebSocket

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
          {stop, Req, State};
        {shutdown, HandlerState2} ->
          {stop, Req, State#state{handler_state=HandlerState2}}
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
      {stop, Req, State};
    {shutdown, HandlerState2} ->
      {stop, Req, State#state{handler_state=HandlerState2}}
end.

terminate(_Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
  Handler:handle(terminate, {HandlerState, Req}),
  ok.
