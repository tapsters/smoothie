-module(sm).
-author("Vitaly Shutko").

%% Setting up
-export([start_http/1]).

%% Formats
-export([json_enc/1]).
-export([json_dec/1]).

%% Utils
-export([env/2]).
-export([env/3]).
-export([env_set/3]).
-export([env_set/4]).
-export([env_set/5]).

-export([prop/2]).
-export([prop/3]).
-export([prop_replace/3]).

-export([hex_to_bin/1]).
-export([bin_to_hex/1]).

%% Debug
-export([stacktrace/0]).

%% Setting up

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

route({Pattern, {file, Path}})                   -> {Pattern, cowboy_static, {file, Path}};
route({Pattern, {dir, Path}})                    -> {Pattern, cowboy_static, {dir, Path}};
route({Pattern, {priv_file, App, Path}})         -> {Pattern, cowboy_static, {priv_file, App, Path}};
route({Pattern, {priv_dir, App, Path}})          -> {Pattern, cowboy_static, {priv_dir, App, Path}};
route({Pattern, {request, Module, Function}})    -> {Pattern, sm_request,   [{module, Module},
                                                                             {function, Function}]};
route({Pattern, {websocket, Handler, Protocol}}) -> {Pattern, sm_websocket, [{handler, Handler},
                                                                             {protocol, Protocol},
                                                                             {timeout, 60000}]};
route({Pattern, {websocket, Handler, Protocol, Timeout}}) -> {Pattern, sm_websocket,
                                                              [{handler, Handler},
                                                               {protocol, Protocol},
                                                               {timeout, Timeout}]}.

routes(Routes) ->
  Routes1 = [route(Route) || Route <- Routes],
  cowboy_router:compile(
    [{'_', [route({"/sm/[...]",   {priv_dir, smoothie, "static"}})]++Routes1}]).

%% Formats

json_enc(Data) ->
  yaws_json2:encode(Data).

json_dec(Data) ->
  {ok, Document} = yaws_json2:decode_string(Data), Document.

%% Utils

env(App, Key)          -> env(App, Key, undefined).
env(App, Key, Default) ->
  application:get_env(App, Key, Default).

env_set(App, Key, Value)                      -> env_set(App, Key, Value, 5000, false).
env_set(App, Key, Value, Timeout)             -> env_set(App, Key, Value, Timeout, false).
env_set(App, Key, Value, Timeout, Persistent) ->
  application:set_env(App, Key, Value, [{timeout, Timeout}, {persistent, Persistent}]).

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

%%(c) Steve Vinoski
bin_to_hex(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

hex_to_bin(List)         -> hex_to_bin(List, []).
hex_to_bin([], Acc)      -> list_to_binary(lists:reverse(Acc));
hex_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hex_to_bin(T, [V | Acc]).

%% Debug

stacktrace() ->
  Trace = try throw(sm_stacktrace) catch sm_stacktrace -> erlang:get_stacktrace() end,
  erlang:display(Trace).
