%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/1]).

start(RobotId) ->
  lager:info("[Robot-~p] Starting...~n", [RobotId]),

  MessageDealer = spawn_link(message_dealer, start, [RobotId]),

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  true = register(ReplyCallback, spawn_link(robot_callback, start, [RobotId, MessageDealer, self()])),

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  true = register(Heartbeat, spawn_link(heartbeat, start, [RobotId, MessageDealer])),

  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer ! {send, TransUnit},
  lager:info("[Robot-~p] Trying to login.~n", [RobotId]),

  loop(RobotId, MessageDealer).

loop(RobotId, MessageDealer) ->
  receive
    {logined, AccountId} ->
      lager:info("[Robot-~p] Login ok, accountId = ~p.~n", [RobotId, AccountId]),
      loop(RobotId, MessageDealer);
    stop ->
      terminate(RobotId, MessageDealer)
  end.

terminate(RobotId, MessageDealer) ->
  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  Heartbeat ! stop,

  MessageDealer ! stop,

  gen_server:cast(robot_scheduler, {return_robot, RobotId}),
  gen_server:cast(robot_scheduler, {start_robot, 1}),
  ok.
