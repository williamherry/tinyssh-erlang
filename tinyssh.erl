-module(tinyssh).
-export([download/6]).
-export([upload/6]).

download(Host, Port, User, Pass, From, To) ->
  case ssh_sftp:start_channel(Host, Port, [{user, User}, {password, Pass}]) of
    {ok, Pid, _} ->
      {ok, Data} = ssh_sftp:read_file(Pid, From),
      file:write_file(To, Data);
    {error, Reason} ->
      io:format("Some thing goes wrong: ~w~n", [Reason])
  end.

upload(Host, Port, User, Pass, From, To) ->
  case ssh_sftp:start_channel(Host, Port, [{user, User}, {password, Pass}]) of
    {ok, Pid, _} ->
      {ok, Data} = file:read_file(From),
      ok = ssh_sftp:write_file(Pid, To, Data);
    {error, Reason} ->
      io:format("Some thing goes wrong: ~w~n", [Reason])
  end.
