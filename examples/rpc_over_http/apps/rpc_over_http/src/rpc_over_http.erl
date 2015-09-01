-module(rpc_over_http).
-author("Vitaly Shutko").
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_, _) ->
  supervisor:start_link({local, rpc_over_http_sup}, ?MODULE, []).

stop(_) ->
  ok.

init([]) ->
  sm:start_http([{ranch, sm:env(smoothie, ranch)},
                 {cowboy, sm:env(smoothie, cowboy)},
                 {routes, routes()}]),

  {ok, {#{strategy => one_for_one, 
          intensity => 5, 
          period => 10}, []}}.

routes() ->
  [{"/",         {priv_file, rpc_over_http,        "static/index.html"}},
   {"/user",     {request,   rpc_over_http_api,    get_all_users      }},
   {"/user/:id", {request,   rpc_over_http_api,    get_user           }}].
