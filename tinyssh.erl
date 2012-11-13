-module(tinyssh).

-export([main/2]).
-export([download/6]).
-export([upload/6]).
-export([shell/4]).
-export([help/0]).

main(Action, IP_FILE) ->
  case Action of
    "download" -> do_download(IP_FILE);
    "upload" -> do_upload(IP_FILE);
    "run" -> do_run(IP_FILE);
    _ -> help()
  end.

help() ->
  io:format("wrong arguments!\nusage:\n\ttinyssh run 'cmd' -l ip_list.txt\n\ttinyssh upload|download src dst\n", []).

do_run(IP_FILE) ->
  io:format("we are run the command ~p~n", [IP_FILE]).

do_download(IP_FILE) ->
  Lines = readlines(IP_FILE),
  Fun = fun(L) -> download(string:strip(L, right, $\n), 22, "william", "william", "/home/william/.bashrc", string:concat("/tmp/.bashrc", L)) end,
  lists:foreach(Fun, Lines).

do_upload(IP_FILE) ->
  Lines = readlines(IP_FILE),
  Fun = fun(L) ->
      io:format("uploading file ~p to ~p\n", ["/home/william/.bashrc", string:concat("/tmp/.bashrc-", L)]),
      upload(string:strip(L, right, $\n), 22, "william", "william", "/home/william/.bashrc", string:concat("/tmp/.bashrc", L))
  end,
  lists:foreach(Fun, Lines).

download(Host, Port, User, Pass, From, To) ->
  case ssh_sftp:start_channel(Host, Port, [{user, User}, {password, Pass}, {silently_accept_hosts, true}]) of
    {ok, Pid, _} ->
      {ok, Data} = ssh_sftp:read_file(Pid, From),
      file:write_file(To, Data);
    {error, Reason} ->
      io:format("Some thing goes wrong: ~w~n", [Reason])
  end.

upload(Host, Port, User, Pass, From, To) ->
  case ssh_sftp:start_channel(Host, Port, [{user, User}, {password, Pass}, {silently_accept_hosts, true}]) of
    {ok, Pid, _} ->
      {ok, Data} = file:read_file(From),
      ok = ssh_sftp:write_file(Pid, To, Data);
    {error, Reason} ->
      io:format("Some thing goes wrong: ~w~n", [Reason])
  end.

shell(Host, Port, User, Pass) ->
  ssh:shell(Host, Port, [{user, User}, {password, Pass}]).

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    eof  -> file:close(Device), Accum;
    Line -> get_all_lines(Device, Accum ++ [Line])
  end.
