%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-include("rpc_pb.hrl").

%% API
-export([init/2]).

%% For internal usage only.
-export([create_robot/1]).

init(RobotStartId, RobotCount) when is_integer(RobotStartId), is_integer(RobotCount), RobotStartId > 0, RobotCount > 0 ->
  lager:start(),
  init(RobotStartId, RobotCount, 0).

init(RobotStartId, RobotCount, N) ->
  case N < RobotCount of
    true ->
      spawn_link(?MODULE, create_robot, [RobotStartId + N]),
      init(RobotStartId, RobotCount, N + 1);
    false ->
      ok
  end.

create_robot(RobotId) ->
  lager:info("create robot id = " ++ integer_to_list(RobotId)),
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, dealer),
  ok = erlzmq:setsockopt(Socket, identity, pid_to_list(self())),
  ServerAddr = "tcp://10.10.10.10:5570",
%%   ServerAddr = "tcp://10.10.9.116:5570",
%%   ServerAddr = "tcp://127.0.0.1:5570",
  ok = erlzmq:connect(Socket, ServerAddr),
  TransUnit = rpc_req:login_req(RobotId),
%%   TransUnit = rpc_req:create_account_req(RobotId),
  send(Socket, TransUnit),
  close(Socket),
  terminate(Context).

send(Socket, TransUnit) ->
  Bin = list_to_binary(rpc_pb:encode_transunit(TransUnit)),
  erlzmq:send(Socket, Bin),
  loop(Socket, 3).

loop(_Socket, 0) ->
  over;
loop(Socket, N) when N > 0 ->
  case polling(Socket, 100, 10) of
    {error, timeout} ->
      loop(Socket, N - 1);
    {ok, ReplyBin} ->
      TransUnit = rpc_pb:decode_transunit(ReplyBin),
      case rpc_pb:get_extension(TransUnit, loginreply) of
        {ok, LoginReply} ->
          Id = (LoginReply#loginreply.accountinfo)#accountinfo.id,
          lager:info("Robot ~p loged in.~n", [Id]);
        undefined ->
          lager:info("Robot FAILED to login: ~p~n", [TransUnit])
      end,
      loop(Socket, N)
  end.

polling(_Socket, 0, _Delay) ->
  {error, timeout};
polling(Socket, N, Delay) when N > 0 ->
  case erlzmq:recv(Socket, [dontwait]) of
    {ok, Msg} ->
      {ok, Msg};
    {error, eagain} ->
      timer:sleep(Delay),
      polling(Socket, N - 1, Delay)
  end.

close(Socket) ->
  erlzmq:close(Socket).

terminate(Context) ->
  erlzmq:term(Context).