-module(sm_proto).
-author("Vitaly Shutko").

-export([encode/2, decode/2]).

-callback handle_encode(any())
  -> {ok, any()}
  | {error, list()}.

-callback handle_decode(any())
  -> {ok, any()}
  | {error, list()}.

encode(Data, Protocol) -> Protocol:handle_encode(Data).
decode(Data, Protocol) -> Protocol:handle_decode(Data).