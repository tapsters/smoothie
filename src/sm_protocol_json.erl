-module(sm_protocol_json).
-author("Vitaly Shutko").
-author("Oleg Zinchenko").
-behaviour(sm_protocol).

-export([supports_format/1, encode/2, decode/2]).

supports_format(Format) ->
  case Format of
    text -> true;
    _    -> false
  end.

encode(Data, text) ->
  yaws_json2:encode(Data).

decode(Data, text) ->
  {ok, Document} = yaws_json2:decode_string(Data), Document.
