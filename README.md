Smoothie
========

Cute web framework for Erlang built on top of Cowboy.

Installing
----------

In rebar.config:

```Erlang
{smoothie, ".*", {git, "git://github.com/tapsters/smoothie", {tag, "master"}}}
```

Starting up
-----------

Starting http server:

```Erlang
sm:start_http([
  {ranch, [{port, 3000}]},
  {cowboy, [
    {nb_acceptors, 100},
    {protocol, [{compress, true}]}
  ]},
  {routes, [
    {"/",         {priv_file, my_app, "static/index.html"}},
    {"/js/[...]", {priv_dir, "static/js"}},
    {"/user/:id", {request,   my_api,       get_user}},
    {"/ws",       {websocket, my_websocket, sm_protocol_bert}}
  ]}
]).
```

More about configuring Ranch TCP transport and Cowboy protocol: 
[ranch\_tcp](http://ninenines.eu/docs/en/ranch/HEAD/manual/ranch_tcp/), 
[cowboy\_protocol](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_protocol/).

In the above example `request` and `websocket` are special route types provided
by Smoothie to make handling HTTP requests and WebSockets easier.

More about Cowboy's routing:
[Routing](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing), 
[Constraints](http://ninenines.eu/docs/en/cowboy/HEAD/guide/constraints), 
[Static files](http://ninenines.eu/docs/en/cowboy/HEAD/guide/static_files).

Handling HTTP requests
----------------------

To handle HTTP request you should specify route for it:

```Erlang
sm:start_http([
  {routes, [
    {"/user",     {request, my_api, get_all_users}},
    {"/user/:id", {request, my_api, get_user}}
  ]}
]).
```

In the above example `my_api` is module name and `get_all_user`/`get_user` are
function names.

Example code for `my_api` module:

```Erlang
-module(my_api).

-include_lib("smoothie/include/sm.hrl").

-export([get_all_users/1, get_user/1]).

get_all_users(_Req) ->
  Cookie = #sm_cookie{name    = <<"some_cookie">>, 
                      value   = <<"some_value">>, 
                      domain  = <<"example.com">>,
                      path    = <<"/some/path">>,
                      max_age = 3600},

  {ok, #sm_response{status  = 200, 
                    headers = [{<<"content-type">>, <<"text/plain">>}], 
                    body    = <<"1, 2, 3">>,
                    cookies = [Cookie]}}.

get_user(Req) ->
  {Id, _} = cowboy_req:binding(id, Req),

  {ok, #sm_response{status  = 200, 
                    headers = [{<<"content-type">>, <<"text/plain">>}], 
                    body    = Id}}.
```

More about accessing request's information: 
[cowboy_req](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_req/).

Handling WebSockets
-------------------

To handle WebSocket connections you should implement `sm_websocket` behaviour:

````Erlang
-module(my_websocket).
-behaviour(sm_websocket).
-include_lib("kvs/include/user.hrl").

-export([handle/2]).

-record(state, {
  user
}).

handle(init, {_, WebSocketState}) ->
  Id = sm:qs_val(id, WebSocketState),
  User = kvs:get(user, Id),
  {ok, #state{user=User}};

handle({stream, {text, <<"get_name">>}}, {State=#state{user=User}, _}) ->
  {reply, User#user.username, State};

handle({stream, {_Format, Data}}, {State, _}) -> 
  io:format("Got new data: ~p~n", [Data]),
  {ok, State};

handle(terminate, _) -> ok;
handle(_, _)         -> ok.
````

And also you should define route for your handler:

````Erlang
sm:start_http([
  {routes, [
    {"/ws", {websocket, my_websocket, sm_protocol_relay}}
  ]}
]).
````

In the above example third element of the tuple is data encoding/decondig protocol.
Smoothie implements following protocols:
* relay - passes data as is
* bert - uses BIFs to encode/decode data
[BERT](http://bert-rpc.org) which is compatible with Erlang's 
[ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
* json - uses [yaws_json2](https://github.com/tapsters/yaws-json2) to encode/decode data

If you want add your own protocol, you should implement `sm_protocol` behaviour.

Working with JSON
-----------------

Smoothie uses [yaws_json2](https://github.com/tapsters/yaws-json2) taked
from [Yaws](https://github.com/klacke/yaws) webserver to encode/decode JSON.

Use `sm:json_enc/1` and `sm:json_dec/1` to encode/decode json. 

Decode example (encode works in a same way):

````JavaScript
{
  first_name: "John",
  last_name: "Smith",
  enabled: true,
  phone_number: 937600131,
  avatar: {
    origin: "default.png",
    thumbnails: [
      "default_min.png"
    ]
  }
}
````

Result:

````Erlang
{struct, [
  {"first_name", "John"},
  {"last_name", "Smith"},
  {"enabled", true},
  {"phone_number", 937600131},
  {"avatar", {struct, [
    {"origin", "default.png"},
    {"thumbnails", {array, [
      "default_min.png"
    ]}}
  ]}}
]}
````

Client-side
-----------

To use Smoothie in browser include following dependencies:

````HTML
<script type="text/javascript" src="/sm/erlb.js"></script>
<script type="text/javascript" src="/sm/smoothie.js"></script>
````

Now you can esteblish WebSocket connection, send and receive data from server:

````JavaScript
var ws = Smoothie.connect({
  protocol: "bert",
  onMessage: function(data) {
    console.log("Got message:", data);
  },
});

ws.send(Erl.tuple(Erl.atom("ok"), "Hello", 123));
````

Detailed example of the `Smoothie.connect` options:

````JavaScript
{
  http: {
    protocol:     "ws://",
    host:         "example.com",
    port:         3000,
    path:         "/ws",
  },
  
  queryParams:    {token: 987654321},
  
  heartBeatDelay: 20000,
  protocol:       "relay",

  onOpen:         function() {},
  onMessage:      function(data) {},
  onDisconnect:   function() {},
  onClose:        function() {},

  onBeforeSend:   function(data) {}
}
````

When you send or receive data from server, it is encoded/decoded by choosen protocol.
Smoothie implements following protocols:
* relay - passes data as is
* bert - uses [Erlb.js](https://github.com/saleyn/erlb.js) to encode/decode data to 
[BERT](http://bert-rpc.org) which is compatible with Erlang's 
[ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
* json - uses JSON to encode/decode data


