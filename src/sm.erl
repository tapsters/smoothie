-module(sm).
-author("Vitaly Shutko").

-include("sm.hrl").

%% Setting Up
-export([start_http/1]).
-export([route/2]).

%% WebSocket state
-export([qs/1]).
-export([qs_val/2]).
-export([qs_vals/1]).

%% Formats
-export([json_enc/1]).
-export([json_dec/1]).

%% Config
-export([env/2]).
-export([env/3]).
-export([env_set/3]).
-export([env_set/4]).
-export([env_set/5]).

%% Utils
-export([prop/2]).
-export([prop/3]).
-export([prop_replace/3]).

%% Debug
-export([stacktrace/0]).

%% Setting Up

start_http(Opts) ->
  TransOpts     = prop(ranch,  Opts, [{port, 3000}]),
  CowboyOpts    = prop(cowboy, Opts, [{nb_acceptors, 100}, 
                                      {protocol, [{env, []}]}]),
  Routes        = prop(routes, Opts, []),
  
  NbAcceptors   = prop(nb_acceptors, CowboyOpts),
  ProtoOpts     = prop(protocol,     CowboyOpts),
  
  ProtoEnvOpts  = prop_replace(dispatch, prop(env, ProtoOpts, []), routes(Routes)),
  ProtoOpts2    = prop_replace(env,      ProtoOpts, ProtoEnvOpts),

  cowboy:start_http(http, NbAcceptors, TransOpts, ProtoOpts2).

route(Pattern, {file, Path})            -> {Pattern, cowboy_static, {file, Path}};
route(Pattern, {dir, Path})             -> {Pattern, cowboy_static, {dir, Path}};
route(Pattern, {priv_file, App, Path})  -> {Pattern, cowboy_static, {priv_file, App, Path}};
route(Pattern, {priv_dir, App, Path})   -> {Pattern, cowboy_static, {priv_dir, App, Path}};
route(Pattern, {websocket, Handler, Protocol}) ->
  {Pattern, sm_websocket, [{handler, Handler}, {protocol, Protocol}]};
route(Pattern, {ws, Handler, Protocol, Timeout}) ->
  {Pattern, sm_websocket, [{handler, Handler}, {protocol, Protocol}, {timeout, Timeout}]}.

routes(Routes) ->
  cowboy_router:compile(
    [{'_', [route("/sm/erlb.js", {file, "deps/erlb/erlb.js"}),
            route("/sm/[...]",   {priv_dir, smoothie, "static"})]++Routes}]).

to_binary(Value) when is_atom(Value) -> list_to_binary(atom_to_list(Value));
to_binary(Value)                     -> Value.

%% WebSocket state

qs(#sm_websocket_state{req=Req}) ->
  {Qs, _} = cowboy_req:qs(Req), Qs.

qs_val(Name, #sm_websocket_state{req=Req}) ->
  {Value, _} = cowboy_req:qs_val(to_binary(Name), Req), Value.

qs_vals(#sm_websocket_state{req=Req}) ->
  {List, _} = cowboy_req:qs_vals(Req), List.

%% Formats

json_enc(Data) -> 
  yaws_json2:encode(Data).

json_dec(Data) -> 
  {ok, Document} = yaws_json2:decode_string(Data), Document.

%% Config

env(App, Key)          -> env(App, Key, undefined).
env(App, Key, Default) ->
  application:get_env(App, Key, Default).

env_set(App, Key, Value)                      -> env_set(App, Key, Value, 5000, false).
env_set(App, Key, Value, Timeout)             -> env_set(App, Key, Value, Timeout, false).
env_set(App, Key, Value, Timeout, Persistent) ->
  application:set_env(App, Key, Value, [{timeout, Timeout}, {persistent, Persistent}]).

%% Utils

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

%% Debug

stacktrace() ->
  Trace = try throw(sm_stacktrace) catch sm_stacktrace -> erlang:get_stacktrace() end,
  erlang:display(Trace).