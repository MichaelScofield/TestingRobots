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
  MessageDealer = list_to_atom("robot_md" ++ integer_to_list(RobotId)),
  true = register(MessageDealer, spawn_link(message_dealer, start, [RobotId, self()])),
  Heartbeat = list_to_atom("robot_hb" ++ integer_to_list(RobotId)),
  true = register(Heartbeat, spawn_link(heartbeat, start, [RobotId, MessageDealer])),
  TransUnit = rpc_req:login_req(RobotId),
%%   TransUnit = rpc_req:create_account_req(RobotId),
  MessageDealer ! {send, TransUnit},
  receive
    {received, ReplyBin} ->
      ReplyMsg = rpc_pb:decode_transunit(ReplyBin),
      case rpc_pb:get_extension(ReplyMsg, loginreply) of
        {ok, LoginReply} ->
          Id = (LoginReply#loginreply.accountinfo)#accountinfo.id,
          lager:info("Robot ~p loged in.~n", [Id]);
        undefined ->
          lager:info("Robot FAILED to login: ~p~n", [ReplyMsg])
      end
  after 60000 ->
    lager:error("robot ~p receiving timeout", [RobotId])
  end,
  Heartbeat ! stop,
  MessageDealer ! stop.