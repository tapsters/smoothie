-module(sm_protocol).
-author("Vitaly Shutko").

-type format() :: 'text' | 'binary'.
-type data() :: any().

-callback supports_format(format())
  -> boolean().

-callback encode(data(), format())
  -> {ok, data()}
  | {error, list()}.

-callback decode(data(), format())
  -> {ok, data()}
  | {error, list()}.
