-module(sm).
-author("Vitaly Shutko").

%% Setting up
-export([start_http/1]).
-export([route/2]).

%% Request
-export([qs/1]).
-export([qs_val/2]).
-export([qs_vals/1]).

%% Utils
-export([prop/2]).
-export([prop/3]).
-export([prop_replace/3]).

%% Temp
-export([handle/2]).

start_http(Opts) ->
  TransOpts = prop(ranch, Opts, [{port, 3000}]),
  CowboyOpts = prop(cowboy, Opts, [{nb_acceptors, 100}, 
                                   {routes, []},
                                   {protocol, [{env, []}]}]),
  NbAcceptors = prop(nb_acceptors, CowboyOpts),
  Routes = prop(routes, CowboyOpts),
  ProtoOpts = prop(protocol, CowboyOpts),
  ProtoEnvOpts = prop(env, ProtoOpts),
  ProtoOpts2 = prop_replace(env, ProtoOpts, prop_replace(dispatch, ProtoEnvOpts, routes(Routes))),

  cowboy:start_http(http, NbAcceptors, TransOpts, ProtoOpts2).


route(Pattern, {file, Path})            -> {Pattern, cowboy_static, {file, Path}};
route(Pattern, {dir, Path})             -> {Pattern, cowboy_static, {dir, Path}};
route(Pattern, {priv_file, App, Path})  -> {Pattern, cowboy_static, {priv_file, App, Path}};
route(Pattern, {priv_dir, App, Path})   -> {Pattern, cowboy_static, {priv_dir, App, Path}};
route(Pattern, {ws, Handler, Protocol}) ->
  {Pattern, sm_ws_handler, [{handler, Handler}, {protocol, Protocol}]};
route(Pattern, {ws, Handler, Protocol, Timeout}) ->
  {Pattern, sm_ws_handler, [{handler, Handler}, {protocol, Protocol}, {timeout, Timeout}]}.

routes(Routes) ->
  cowboy_router:compile(
    [{'_', [route("/",             {file, "priv/static/example.html"}),
            route("/sm/erlb.js",   {file, "deps/erlb/erlb.js"}),
            route("/sm/bullet.js", {priv_file, bullet, "bullet.js"}),
            route("/sm/[...]",     {dir, "priv/static"}),
            route("/ws/[...]",     {ws, ?MODULE, sm_proto_relay})
    ]++Routes}]).

handle({stream, Data}, _) -> {reply, <<"bar">>, undefined};
handle(init, _)           -> io:format("HANDLED INIT~n");
handle(terminate, _)      -> io:format("HANDLED TERMINATE~n");
handle(Message, _)        -> io:format("HANDLED UNKNOWN: ~p~n", [Message]).

to_binary(Value) when is_atom(Value) -> list_to_binary(atom_to_list(Value));
to_binary(Value) -> Value.

qs(Req) ->
  {Qs, _} = cowboy_req:qs(Req), Qs.

qs_val(Name, Req) ->
  {Value, _} = cowboy_req:qs_val(to_binary(Name), Req), Value.

qs_vals(Req) ->
  {List, _} = cowboy_req:qs_vals(Req), List.

prop(Key, List) -> prop(Key, List, none).
prop(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {_, Value} -> Value;
    _ -> Default
  end.

prop_replace(Key, List, Value) ->
  case lists:keyfind(Key, 1, List) of
    false -> [{Key, Value}|List];
    _ -> lists:keyreplace(Key, 1, List, {Key, Value})
  end.