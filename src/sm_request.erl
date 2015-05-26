-module(sm_request).
-author("Vitaly Shutko").
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-record(response, {status, headers, body}).
-record(state, {options}).

init({_Transport, _Protocol}, Req, Opts) ->
  {ok, Req, #state{options=Opts}}.

get_response_value(Value, Default) ->
  case Value of
    undefined -> Default;
    _         -> Value
  end.

get_response(#response{status=Status, headers=Headers, body=Body}, Req) ->
  Status1  = get_response_value(Status, 200),
  Headers1 = get_response_value(Headers, []),
  Body1    = get_response_value(Body, <<>>),

%%  io:format("REQ: ~p~n", [Req]),
%%  io:format("ARGS: ~p~n", [[Status1, Headers1, Body1]]),
                                                                          
  {ok, Req2} = cowboy_req:reply(Status1, Headers1, Body1, Req),
  Req2.

handle(Req, State=#state{options=Opts}) ->
  {module, Module}     = proplists:lookup(module, Opts),
  {function, Function} = proplists:lookup(function, Opts),
  
  case Module:Function(Req) of
    ok ->
      {ok, get_response(#response{status=200}, Req), State};
    {ok, Status} -> 
      {ok, get_response(#response{status=Status}, Req), State};
    {ok, Status, Headers} -> 
      {ok, get_response(#response{status=Status, headers=Headers}, Req), State};
    {ok, Status, Headers, Body} -> 
%%      io:format("RESPONSE: ~p~n", [{ok, get_response(#response{status=Status, headers=Headers, body=Body}, Req), State}]),
      {ok, get_response(#response{status=Status, headers=Headers, body=Body}, Req), State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
