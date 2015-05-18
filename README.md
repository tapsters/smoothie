# Smoothie
Cute web framework for Erlang built on top of Cowboy and Bullet.

----
## Installing

In rebar.config:

    {smoothie,  ".*", {git, "git://github.com/myua/smoothie", {tag, "master"} }}

----
## Usage

Starting http server:

```Erlang
    sm:start_http([
      {ranch, [{port, 3000}]},
      {cowboy, [
        {nb_acceptors, 100},
        {rules, [
          sm:route("/",         {priv_file, myapp, "static/index.html"}),
          sm:route("/js/[...]", {priv_dir, "staric/js"}),
          sm:route("/ws/[...]", {ws, my_handler})
        ]},
        {protocol, [{compress, true}]}
      ]}
    ])
```

More about configuring Ranch TCP transport and Cowboy protocol: 
[ranch\_tcp](http://ninenines.eu/docs/en/ranch/HEAD/manual/ranch_tcp/), 
[cowboy\_protocol](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_protocol/).

More about routing rules:
[Routing](http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing), 
[Constraints](http://ninenines.eu/docs/en/cowboy/HEAD/guide/constraints), 
[Static files](http://ninenines.eu/docs/en/cowboy/HEAD/guide/static_files).
