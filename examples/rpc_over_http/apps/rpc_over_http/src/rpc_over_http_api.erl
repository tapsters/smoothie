-module(rpc_over_http_api).
-author("Vitaly Shutko").

-include_lib("smoothie/include/sm.hrl").

-export([get_all_users/1, get_user/1]).

get_all_users(_Req) ->
  Body = <<"All users">>,
  Cookie = #sm_cookie{name    = <<"some_cookie">>, 
                      value   = <<"some_value">>, 
                      max_age = 3600},

  {ok, #sm_response{status  = 200, 
                    headers = [{<<"content-type">>, <<"text/plain">>}], 
                    body    = Body,
                    cookies = [Cookie]}}.

get_user(Req) ->
  Id = cowboy_req:binding(id, Req),
  Body = list_to_binary("User id: " ++ binary_to_list(Id)),

  {ok, #sm_response{status  = 200, 
                    headers = [{<<"content-type">>, <<"text/plain">>}], 
                    body    = Body}}.