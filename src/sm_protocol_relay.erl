-module(sm_protocol_relay).
-author("Vitaly Shutko").
-behaviour(sm_protocol).

-export([supports_format/1, encode/2, decode/2]).

supports_format(_Format) -> true.

encode(Data, _Format) -> Data.
decode(Data, _Format) -> Data.
