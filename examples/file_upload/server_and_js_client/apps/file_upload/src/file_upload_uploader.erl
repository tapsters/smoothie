-module(file_upload_uploader).
-author("Oleg Zinchenko").
-author("Vitaly Shutko").

-behaviour(sm_websocket).

-export([handle/2]).

-define(LIMIT, 10485760).

-record(state, {file, 
                size,
                name,
                ext,
                path}).

handle(init, {_, Req}) ->
  {Mega, Sec, Micro} = os:timestamp(),
  Rnd = rnd:string(12, chars_and_numbers),
  Name = Rnd ++ integer_to_list(Mega) ++ integer_to_list(Sec) ++ integer_to_list(Micro),
  QS = cowboy_req:parse_qs(Req),
  Ext = sm:prop(<<"ext">>, QS),
  Path = "/tmp/" ++ Name,
  file:delete(Path),

  {ok, File} = file:open(Path, [write, binary]),

  self() ! message(ready),

  {ok, #state{size = 0,
              file = File,
              path = Path,
              name = Name,
              ext  = binary_to_list(Ext)}};

handle({stream, Data}, {State, _Req}) ->
  case Data of
    {binary, <<"done">>} ->
      file:close(State#state.file),
      Url = "/uploads/" ++ State#state.name ++ "." ++ State#state.ext,
      file:copy(State#state.path, code:priv_dir(file_upload) ++ Url),
      {reply, term_to_binary(message(complete, Url)), State};
    {binary, Binary} ->
      NewSize = State#state.size + byte_size(Binary),
      case NewSize >= ?LIMIT of
        true  -> {shutdown, State};
        false ->
          ok = file:write(State#state.file, Binary),
          {ok, State#state{size = NewSize}}
      end
  end;

handle({info, Info = [{status, <<"ready">>}]}, {State, _Req}) ->
  {reply, {binary, term_to_binary(Info)}, State};

handle(terminate, {State, _Req})  ->
  file:close(State#state.file),
  file:delete(State#state.path),
  ok;

handle(_, _) -> ok.

message(Status)       -> message(Status, undefined).
message(Status, Data) ->
  Res = [{status, list_to_binary(atom_to_list(Status))}],
  case Data of
    undefined -> Res;
    Data2     -> Res ++ [{data, list_to_binary(Data2)}]
  end.
