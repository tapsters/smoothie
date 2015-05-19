-module(sm_app).
-author("Vitaly Shutko").
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_,_) ->
  supervisor:start_link({local, sm_sup}, ?MODULE, []).

stop(_) ->
  ok.

init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
