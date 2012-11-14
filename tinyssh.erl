-module(tinyssh).

-export([main/1]).
-export([download/3]).
-export([upload/3]).
-export([shell/4]).
-export([help/0]).
-export([run_action/7]).

main([Host_file, Port, User, Pass, Action, From, To]) ->
  crypto:start(),
  ssh:start(),

  Hosts = readlines(Host_file),
  Fun = fun(Host) ->
      run_action(Host, Port, User, Pass, Action, From, To)
  end,
  lists:foreach(Fun, Hosts).

run_action(Host, Port, User, Pass, Action, From, To) ->
  try connect(Host, list_to_integer(Port), User, Pass) of
    {ok, Connection} ->
      io:format("~nConnected to remote host: ~p~n", [Host]),
      erlang:apply(tinyssh, list_to_atom(Action),
                   [Connection, From, To]);
    {error, Reason} ->
      io:format("Error occur when connecting to host: ~w~n", [Reason])
  catch
    Other ->
      io:format("Error occur when connecting to host: ~w~n", [Other])
  end.

connect(Host, Port, User, Pass) ->
  ssh:connect(Host, Port, [{user, User},
                               {password, Pass},
                               {silently_accept_hosts, true},
                               3
                              ]).

download(Connection, From, To) ->
  case ssh_sftp:start_channel(Connection) of
    {ok, Pid} ->
      case ssh_sftp:read_file(Pid, From) of
        {ok, Data} ->
          case file:write_file(To, Data) of
            ok ->
              io:format("Successfully downloaded ~p to ~p~n~n",
                        [From, To]);
            {error, Reason} ->
              io:format("Error occur when write to local file: ~w~n", [Reason])
          end;
        {error, Reason} ->
          io:format("Error occur read from remote file : ~w~n", [Reason])
      end;
    {error, Reason} ->
      io:format("Error occur when start channel: ~w~n", [Reason])
  end.

upload(Connection, From, To) ->
  case ssh_sftp:start_channel(Connection) of
    {ok, Pid} ->
      case file:read_file(From) of
        {ok, Data} ->
          case ssh_sftp:write_file(Pid, To, Data) of
            ok ->
              io:format("Successfully uploaded ~p to ~p~n~n",
                        [From, To]);
            {error, Reason} ->
              io:format("Error occur when write to remote file: ~w~n", [Reason])
          end;
        {error, Reason} ->
          io:format("Error occur when read from local file: ~w~n", [Reason])
      end;
    {error, Reason} ->
      io:format("Error occur when uploading: ~w~n", [Reason])
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

