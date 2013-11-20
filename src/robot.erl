%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2]).

start(RobotId, RobotType) ->
  lager:info("[Robot-~p] Initializing...~n", [RobotId]),

  process_flag(trap_exit, true),

  MessageDealer = spawn_link(message_dealer, start, [RobotId, self()]),

  Heartbeat = spawn_link(heartbeat, pow, [RobotId, MessageDealer]),

  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer ! {send, TransUnit},

  loop(RobotId, RobotType, MessageDealer, Heartbeat, null).

loop(RobotId, RobotType, MessageDealer, Heartbeat, RobotTimer) ->
  receive
    {logined, AccountId} ->
      lager:info("[Robot-~p] logined, accountId:~p.~n", [RobotId, AccountId]),
      NewRobotTimer = spawn_link(robot_timer, start, [RobotId, AccountId, MessageDealer]),
      case RobotType of
        arena ->
          MessageDealer ! {send, rpc_req:change_city_req(100002)},
          MessageDealer ! {send, rpc_req:enter_arena_req()},
          lager:info("[Robot-~p] entering arena...");
        idle ->
          ok
      end,
      loop(RobotId, RobotType, MessageDealer, Heartbeat, NewRobotTimer);
    stop ->
      terminate(RobotId, MessageDealer, Heartbeat, RobotTimer);
    {'EXIT', From, Reason} ->
      lager:error("[Robot-~p] EXIT from ~p, reason: ~p", [RobotId, From, Reason]),
      terminate(RobotId, MessageDealer, Heartbeat, RobotTimer)
  after 600000 ->
    lager:warning("[Robot-~p] Timeout", [RobotId]),
    terminate(RobotId, MessageDealer, Heartbeat, RobotTimer)
  end.

terminate(RobotId, MessageDealer, Heartbeat, RobotTimer) ->
  Heartbeat ! stop,

  MessageDealer ! stop,

  case RobotTimer of
    null -> ok;
    Pid -> Pid ! stop
  end,

  gen_server:cast(robot_scheduler, {return_robot, RobotId}),
  gen_server:cast(robot_scheduler, {start_robot, 1}),
  ok.
