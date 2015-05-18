-module(sm).
-author("Vitaly Shutko").

%% Web server
-export([start_http/1]).
-export([route/2]).

%% Utils
-export([prop/2]).
-export([prop/3]).
-export([prop_replace/3]).

start_http(Opts) ->
  TransOpts = prop(ranch, Opts, [{port, 3000}]),
  CowboyOpts = prop(cowboy, Opts, [{nb_acceptors, 100}, 
                                   {routes, []},
                                   {protocol, [{env, []}]}
                                  ]),
  NbAcceptors = prop(nb_acceptors, CowboyOpts),
  Routes = prop(routes, CowboyOpts),
  ProtoOpts = prop(protocol, CowboyOpts),
  ProtoEnvOpts = prop(env, ProtoOpts),
  ProtoOpts2 = prop_replace(env, ProtoOpts, prop_replace(dispatch, ProtoEnvOpts, routes(Routes))),

  cowboy:start_http(http, NbAcceptors, TransOpts, ProtoOpts2).


route(Pattern, {file, Path})           -> {Pattern, cowboy_static, {file, Path}};
route(Pattern, {dir, Path})            -> {Pattern, cowboy_static, {dir, Path}};
route(Pattern, {priv_file, App, Path}) -> {Pattern, cowboy_static, {priv_file, App, Path}};
route(Pattern, {priv_dir, App, Path})  -> {Pattern, cowboy_static, {priv_dir, App, Path}}.
%%route(Pattern, {ws, Handler})          -> {Pattern, bullet_handler, [{handler, sm_bullet_handler}]}.

routes(Routes) ->
  cowboy_router:compile(
    [{'_', [{"/",             cowboy_static,   {file, "priv/static/example.html"}},
            {"/sm/[...]",     cowboy_static,   {dir, "priv/static"}},
            {"/sm/bullet.js", cowboy_static,   {priv_file, bullet, "bullet.js"}},
            {"/ws/[...]",     bullet_handler, [{handler, sm_bullet_handler}]}
%%            {"/favicon.ico", cowboy_static, {priv_file, web, "static/favicon.ico"}},
%%            {"/static/[...]", cowboy_static, {priv_dir, web, "static"}}
    ]++Routes}]).

prop(Key, List) -> prop(Key, List, none).
prop(Key, List, Default) ->
  Result = proplists:lookup(Key, List),
  case Result of
    none -> Default;
    {_, Value} -> Value
  end.

prop_replace(Key, List, Value) ->
  case proplists:lookup(Key, List) of
    none -> [{Key, Value}|List];
    _ -> lists:keyreplace(Key, 1, List, {Key, Value})
  end.