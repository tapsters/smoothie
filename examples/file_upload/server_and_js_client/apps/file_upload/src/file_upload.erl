-module(file_upload).
-author("Vitaly Shutko").
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_, _) ->
  supervisor:start_link({local, file_upload_sup}, ?MODULE, []).

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
  [{"/",       {priv_file, file_upload,          "static/index.html"}},
   {"/upload", {websocket, file_upload_uploader, sm_protocol_relay  }},
   {"/[...]",  {priv_dir,  file_upload,          ""                 }}].
