-module(sm_ws_handler).
-author("Vitaly Shutko").
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-include("sm.hrl").

-export([init/3, handle/2, info/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(state, {
  handler,
  handler_state
}).

-type handler_state() :: any().
-type websocket_state() :: #sm_ws_state{}.
-type state() :: {handler_state(), websocket_state()}.
-type data() :: any().
-type message() :: init
  | {stream, data()}
  | {info, any()}
  | terminate.

-callback handle(message(), state())
	-> any().

%% HTTP

init(_Transport, Req, _Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {undefined, Req2} ->
      {shutdown, Req2, undefined};
    {Bin, Req2} when is_binary(Bin) ->
      case cowboy_bstr:to_lower(Bin) of
        <<"websocket">> ->
          {upgrade, protocol, cowboy_websocket};
        _ ->
          {ok, Req3} = cowboy_req:reply(501, [], [], Req2),
          {shutdown, Req3, undefined}
      end;
    {_, Req2} -> {shutdown, Req2, undefined}
  end.

handle(Req, State) ->
  {ok, Req, State}.

info(Message, Req, State) ->
  {ok, Req, State}.

%% WebSocket

make_ws_state(Req) ->
  #sm_ws_state{req=Req}.

websocket_init(_Transport, Req, Opts) ->
  Handler = sm:prop(handler, Opts),
  Timeout = sm:prop(timeout, Opts, 60000),
  State = #state{handler=Handler},
  
  case Handler:handle(init, {undefined, make_ws_state(Req)}) of
    ok ->
      {ok, Req, State#state{handler_state=undefined}, Timeout, hibernate};
    {ok, HandlerState} ->
      {ok, Req, State#state{handler_state=HandlerState}, Timeout, hibernate};
    {shutdown, _HandlerState} ->
      {shutdown, Req}
  end.

websocket_handle({text, <<"ping">>}, Req, State) ->
  {reply, {text, <<"pong">>}, State, hibernate};
websocket_handle(Data, Req, State=#state{handler=Handler, handler_state=HandlerState}) ->
  io:format("FRAME: ~p~n", [Data]),
  case Handler:handle({stream, Data}, {HandlerState, make_ws_state(Req)}) of
    ok ->
      {ok, Req, State, hibernate};
    {ok, HandlerState2} ->
      {ok, Req, State#state{handler_state=HandlerState2}, hibernate};
    {reply, Reply, HandlerState2} ->
      {reply, {text, Reply}, Req, State#state{handler_state=HandlerState2}, hibernate};
    {shutdown, HandlerState2} ->
      {shutdown, Req, State#state{handler_state=HandlerState2}}
  end.

websocket_info(Info, Req, State=#state{handler=Handler, handler_state=HandlerState}) ->
  case Handler:handle({info, Info}, {HandlerState, make_ws_state(Req)}) of
    ok ->
      {ok, Req, State, hibernate};
    {ok, HandlerState2} ->
      {ok, Req, State#state{handler_state=HandlerState2}, hibernate};
    {reply, Reply, HandlerState2} ->
      {reply, {text, Reply}, Req, State#state{handler_state=HandlerState2}, hibernate};
    {shutdown, HandlerState2} ->
      {shutdown, Req, State#state{handler_state=HandlerState2}}
end.

websocket_terminate(_Reason, Req, #state{handler=Handler, handler_state=HandlerState}) ->
  Handler:handle(terminate, {HandlerState, make_ws_state(Req)}).
