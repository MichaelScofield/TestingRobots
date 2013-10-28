%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/1]).

start(RobotId) ->
  lager:info("[Robot-~p] Initializing...~n", [RobotId]),

  process_flag(trap_exit, true),

  MessageDealer = spawn_link(message_dealer, start, [RobotId]),

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  true = register(ReplyCallback, spawn_link(robot_callback, start, [RobotId, MessageDealer, self()])),

  Heartbeat = spawn_link(heartbeat, pow, [RobotId, MessageDealer]),

  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer ! {send, TransUnit},

  loop(RobotId, MessageDealer, Heartbeat).

loop(RobotId, MessageDealer, Heartbeat) ->
  receive
    {logined, AccountId} ->
      lager:info("[Robot-~p] logined, accountId:~p.~n", [RobotId, AccountId]),
      loop(RobotId, MessageDealer, Heartbeat);
    stop ->
      terminate(RobotId, MessageDealer, Heartbeat);
    {'EXIT', From, Reason} ->
      lager:error("[Robot-~p] EXIT from ~p, reason: ~p", [RobotId, From, Reason]),
      terminate(RobotId, MessageDealer, Heartbeat)
  end.

terminate(RobotId, MessageDealer, Heartbeat) ->
  Heartbeat ! stop,

  MessageDealer ! stop,

  gen_server:cast(robot_scheduler, {return_robot, RobotId}),
  gen_server:cast(robot_scheduler, {start_robot, 1}),
  ok.
