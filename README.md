# Smoothie
Cute web framework for Erlang built on top of Cowboy.

----
## Installing

In rebar.config:

```Erlang
{smoothie,  ".*", {git, "git://github.com/myua/smoothie", {tag, "master"} }}
```

----
## Starting up

Starting http server:

```Erlang
sm:start_http([
  {ranch, [{port, 3000}]},
  {cowboy, [
    {nb_acceptors, 100},
    {protocol, [{compress, true}]}
  ]},
  {routes, [
    sm:route("/",         {priv_file, my_app, "static/index.html"}),
    sm:route("/js/[...]", {priv_dir, "staric/js"}),
    sm:route("/ws",       {websocket, my_websocket, sm_protocol_bert})
  ]}
]).
```

More about configuring Ranch TCP transport and Cowboy protocol: 
[ranch\_tcp](http://ninenines.eu/docs/en/ranch/HEAD/manual/ranch_tcp/), 
[cowboy\_protocol](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_protocol/).

More about routing:
[Routing](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing), 
[Constraints](http://ninenines.eu/docs/en/cowboy/HEAD/guide/constraints), 
[Static files](http://ninenines.eu/docs/en/cowboy/HEAD/guide/static_files).

----
## Handling WebSockets

To handle WebSocket connections you should implement sm_websocket behaviour:

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

handle({stream, <<"get_name">>}, {State=#state{user=User}, _}) ->
  {reply, User#user.username, State};

handle({stream, Data}, {State, _}) -> 
  io:format("Got new data: ~p~n", [Data]),
  {ok, State};

handle(terminate, _) -> ok;
handle(_, _)         -> ok.
````

And also you should define route for your handler:

````Erlang
sm:start_http([
  {routes, [
    sm:route("/ws", {websocket, my_websocket, sm_protocol_bert})
  ]}
]).
````

In the above example third element of the tuple is data encoding and decondig protocol.
Smoothie implements following protocols:
* relay - passes data as is
* bert - uses BIFs to encode and decode data to 
[BERT](http://bert-rpc.org) which is compatible with Erlang's 
[ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)

If you want add your own protocol, you should implement sm_protocol behaviour.

----
## Client-side

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

Detailed example of the Smoothie.connect options:

````JavaScript
{
  http: {
    protocol:     "ws://",
    host:         "example.com",
    port:         3000,
    path:         "/ws",
  },
  
  heartBeatDelay: 20000,
  protocol:       "relay",

  onOpen:         function() {},
  onMessage:      function(data) {},
  onDisconnect:   function() {},
  onClose:        function() {},

  onBeforeSend:   function(data) {}
}
````

When you send or receive data from server, it is encoded and decoded by choosen protocol.
Smoothie implements following protocols:
* relay - passes data as is
* bert - uses [Erlb.js](https://github.com/saleyn/erlb.js) to encode and decode data to 
[BERT](http://bert-rpc.org) which is compatible with Erlang's 
[ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)


