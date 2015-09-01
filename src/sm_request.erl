-module(sm_request).
-author("Vitaly Shutko").
-behaviour(cowboy_handler).

-include("sm.hrl").

-export([init/2]).

%% Cowboy Handler Callbacks

init(Req, Opts) ->
  {module, Module}     = proplists:lookup(module, Opts),
  {function, Function} = proplists:lookup(function, Opts),

  case Module:Function(Req) of
    ok ->
      {ok, get_response(#sm_response{status=200}, Req), Opts};
    {ok, Response=#sm_response{}} ->
      {ok, get_response(Response, Req), Opts}
  end.

%% Internal

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
  cowboy_req:reply(Status, Headers, Body, Req2).
