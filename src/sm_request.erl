-module(sm_request).
-author("Vitaly Shutko").
-behaviour(cowboy_http_handler).

-include("sm.hrl").

-export([init/3, handle/2, terminate/3]).

-record(state, {options}).

%% cowboy_http_handler Callbacks

init({_Transport, _Protocol}, Req, Opts) ->
  {ok, Req, #state{options=Opts}}.

handle(Req, State=#state{options=Opts}) ->
  {module, Module}     = proplists:lookup(module, Opts),
  {function, Function} = proplists:lookup(function, Opts),
  
  case proplists:lookup(protocol, Opts) of
    {protocol, Protocol} ->
      Body = get_body(Req),
      {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
      Format = case ContentType of
                 <<"application/octet-stream">> -> binary;
                 <<"application/binary">>       -> binary;
                 _                              -> text
               end,
      Headers = [{<<"content-type">>, ContentType}],
      Decoded = Protocol:decode(Body, Format),

      case Module:Function(Decoded, Req) of
        ok ->
          {ok, get_response(#sm_response{status=200}, Req), State};
        {ok, Reply} ->
          Encoded = Protocol:encode(Reply, Format),
          {ok, get_response(#sm_response{status=200, headers=Headers, body=Encoded}, Req), State};
        {ok, Reply, Response=#sm_response{}} ->
          Encoded = Protocol:encode(Reply, Format),
          Headers1 = Headers ++ Response#sm_response.headers,
          {ok, get_response(Response#sm_response{headers=Headers1, body=Encoded}, Req), State}
      end;
    _ ->
      case Module:Function(Req) of
        ok ->
          {ok, get_response(#sm_response{status=200}, Req), State};
        {ok, Response=#sm_response{}} ->
          {ok, get_response(Response, Req), State}
      end
  end.

terminate(_Reason, _Req, _State) ->
  ok.

%% Internal

get_body(Req) ->
  get_body(Req, []).
get_body(Req, Body) ->
    case cowboy_req:body(Req) of
      {ok, Data, Req1} ->
        Body ++ Data;
      {more, Data, Req1} ->
        get_body(Req1, Body ++ Data)
    end.

add_cookie_option(OptionName, OptionIndex, Cookie, Options) ->
  case element(OptionIndex, Cookie) of
    undefined -> Options;
    Value -> [{OptionName, Value}|Options]
  end.

set_cookies([], Req) -> Req;
set_cookies([Cookie|Cookies], Req) ->
  Name     = Cookie#sm_cookie.name,
  Value    = Cookie#sm_cookie.value,

  Options  = add_cookie_option(max_age,   #sm_cookie.max_age,   Cookie, []),
  Options1 = add_cookie_option(domain,    #sm_cookie.domain,    Cookie, Options),
  Options2 = add_cookie_option(path,      #sm_cookie.path,      Cookie, Options1),
  Options3 = add_cookie_option(secure,    #sm_cookie.secure,    Cookie, Options2),
  Options4 = add_cookie_option(http_only, #sm_cookie.http_only, Cookie, Options3),

  Req2 = cowboy_req:set_resp_cookie(Name, Value, Options4, Req),

  set_cookies(Cookies, Req2).

get_response(#sm_response{status=Status, headers=Headers, body=Body, cookies=Cookies}, Req) ->
  Req2 = set_cookies(Cookies, Req),
  {ok, Req3} = cowboy_req:reply(Status, Headers, Body, Req2),
  Req3.