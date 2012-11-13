-module(tinyssh).

-export([main/1]).
-export([download/3]).
-export([upload/3]).
-export([shell/4]).
-export([help/0]).

main([Host, Post, User, Pass, Action, IP_FILE]) ->
  {ok, Connection} = init(Host, Post, User, Pass),
  case Action of
    "download" ->
      do_download(Connection, IP_FILE);
    "upload" ->
      do_upload(Connection, IP_FILE);
    "run" ->
      do_run(Connection, IP_FILE);
    _ ->
      help()
  end.

init(Host, Port, User, Pass) ->
  {P, []} = string:to_integer(Port),
  crypto:start(),
  ssh:start(),
  ssh:connect(Host, P, [{user, User},
                           {password, Pass},
                           {silently_accept_hosts, true}
                          ]).

do_run(Connection, IP_FILE) ->
  io:format("we are run the command ~p ~p~n", [Connection, IP_FILE]).

do_download(Connection, IP_FILE) ->
  Lines = readlines(IP_FILE),
  Fun = fun(L) ->
      io:format("downloading file ~p to ~p\n",
                ["/home/william/.bashrc", string:concat("/tmp/.bashrc-", L)]),
      download(Connection, "/home/william/.bashrc", string:concat("/tmp/.bashrc", L))
  end,
  lists:foreach(Fun, Lines).

do_upload(Connection, IP_FILE) ->
  Lines = readlines(IP_FILE),
  Fun = fun(L) ->
      io:format("uploading file ~p to ~p\n",
                ["/home/william/.bashrc", string:concat("/tmp/.bashrc-", L)]),
      upload(Connection, "/home/william/.bashrc", string:concat("/tmp/.bashrc", L))
  end,
  lists:foreach(Fun, Lines).

download(Connection, From, To) ->
  case ssh_sftp:start_channel(Connection) of
    {ok, Pid} ->
      {ok, Data} = ssh_sftp:read_file(Pid, From),
      file:write_file(To, Data);
    {error, Reason} ->
      io:format("Some thing goes wrong: ~w~n", [Reason])
  end.

upload(Connection, From, To) ->
  case ssh_sftp:start_channel(Connection) of
    {ok, Pid} ->
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
    Line -> get_all_lines(Device, Accum ++ [string:strip(Line, right, $\n)])
  end.

help() ->
  io:format("wrong arguments!\nusage:\n\t \
            tinyssh run 'cmd' -l ip_list.txt\n\t \
            tinyssh upload|download src dst\n", []).

