-module(sm_proto_relay).
-behaviour(sm_proto).

-export([handle_encode/1, handle_decode/1]).

handle_encode(Data) -> {ok, Data}.
handle_decode(Data) -> {ok, binary_to_list(Data)}.
