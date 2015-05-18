-module(sm_proto_bert).
-behaviour(sm_proto).

-export([handle_encode/1, handle_decode/1]).

handle_encode(Data) ->
  {ok, term_to_binary(Data)}.

handle_decode(Data) ->
  try {ok, binary_to_term(Data, [safe])}
  catch
    error:_ -> {error, "Binary data includes unexisting atoms"}
  end.
