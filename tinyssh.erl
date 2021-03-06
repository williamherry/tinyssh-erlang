-module(tinyssh).

-export([upload_download/1]).
-export([run_cmd/1]).
-export([download/3]).
-export([upload/3]).
-export([run_action/8]).
-export([run_action/6]).
-export([run/2]).
-export([do_exit/0]).
-export([do_exit/1]).

-define(TIMEOUT, 30000).

upload_download([Host_file, Port, User, Pass, Action, From, To]) ->
  crypto:start(),
  ssh:start(),
  Pid = spawn(fun tinyssh:do_exit/0),

  Hosts = readlines(Host_file),
  Fun = fun(Host) ->
      Pid ! {started},
      spawn(?MODULE, run_action, [Host, Port, User, Pass, Action, From, To, Pid])
  end,
  lists:foreach(Fun, Hosts).

run_cmd([Host_file, Port, User, Pass, Action, Cmd]) ->
  crypto:start(),
  ssh:start(),

  Hosts = readlines(Host_file),
  Fun = fun(Host) ->
      spawn(?MODULE, run_action, [Host, Port, User, Pass, Action, Cmd])
  end,
  lists:foreach(Fun, Hosts).

run_action(Host, Port, User, Pass, Action, From, To, Pid) ->
  try connect(Host, list_to_integer(Port), User, Pass) of
    {ok, Connection} ->
      io:format("~nConnected to remote host: ~p~n", [Host]),
      erlang:apply(tinyssh, list_to_atom(Action),
                   [Connection, From, To]),
      Pid ! {done};
    {error, Reason} ->
      io:format("Error occur when connecting to host ~p : ~w~n", [Host, Reason])
  catch
    Other ->
      io:format("Error occur when connecting to host ~p : ~w~n", [Host, Other])
  end.

run_action(Host, Port, User, Pass, Action, Cmd) ->
  try connect(Host, list_to_integer(Port), User, Pass) of
    {ok, Connection} ->
      io:format("~nConnected to remote host: ~p~n", [Host]),
      erlang:apply(tinyssh, list_to_atom(Action),
                   [Connection, Cmd]);
    {error, Reason} ->
      io:format("Error occur when connecting to host ~p : ~w~n", [Host, Reason])
  catch
    Other ->
      io:format("Error occur when connecting to host ~p : ~w~n", [Host, Other])
  end.

connect(Host, Port, User, Pass) ->
  ssh:connect(Host, Port, [{user, User},
                               {password, Pass},
                               {silently_accept_hosts, true},
                               ?TIMEOUT
                              ]).

download(Connection, From, To) ->
  case ssh_sftp:start_channel(Connection) of
    {ok, Pid} ->
      case ssh_sftp:read_file(Pid, From) of
        {ok, Data} ->
          case file:write_file(To, Data) of
            ok ->
              io:format("~nSuccessfully downloaded ~p to ~p~n",
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
              io:format("~nSuccessfully uploaded ~p to ~p~n",
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

run(Connection, Cmd) ->
  case ssh_connection:session_channel(Connection, ?TIMEOUT) of
    {ok, Channel} ->
      case ssh_connection:exec(Connection, Channel, Cmd, ?TIMEOUT) of
        success ->
          io:format("~nSuccessfully run command: ~p~n", [Cmd]);
        fail ->
          io:format("Error occur when run command: ~w", [Cmd])
      end;
    {error, Reason} ->
      io:format("Error occur when session_channel: ~w~n", [Reason])
  end.

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
  case io:get_line(Device, "") of
    eof  -> file:close(Device), Accum;
    Line -> get_all_lines(Device, Accum ++ [string:strip(Line, right, $\n)])
  end.

do_exit() ->
  receive
    {started} ->
      %% io:format("one process started~n"),
      do_exit(1)
  end.

do_exit(0) ->
  io:format("all done~n"),
  init:stop();

do_exit(Pool) ->
  %% io:format("now pool is ~p~n", [Pool]),
  receive
    {started} ->
      %% io:format("one process started~n"),
      do_exit(Pool + 1);
    {done} ->
      %% io:format("one process done~n"),
      do_exit(Pool - 1)
  end.
