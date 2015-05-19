-module(sm_protocol_bert).
-author("Vitaly Shutko").
-behaviour(sm_protocol).

-export([supports_format/1, encode/2, decode/2]).

supports_format(Format) ->
  case Format of 
    binary -> true;
    _ -> false
  end.

encode(Data, binary) ->
  {ok, term_to_binary(Data)}.

decode(Data, binary) ->
  try {ok, binary_to_term(Data, [safe])}
  catch
    error:_ -> {error, "Binary data includes unexisting atoms"}
  end.
