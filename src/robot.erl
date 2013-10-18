%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callback
-export([init/1, logining/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

start_link(RobotId) ->
  RobotFSMId = list_to_atom("robot-fsm-" ++ integer_to_list(RobotId)),
  gen_fsm:start_link({local, RobotFSMId}, robot, RobotId, []).

init(RobotId) ->
  lager:info("[Robot-~p] Robot FSM created.~n", [RobotId]),

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  true = register(ReplyCallback, spawn_link(robot_callback, start, [RobotId])),

  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  true = register(MessageDealer, spawn_link(message_dealer, start, [RobotId, ReplyCallback])),

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  true = register(Heartbeat, spawn_link(heartbeat, start, [RobotId, MessageDealer])),

  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer ! {send, TransUnit},
  lager:info("[Robot-~p] Trying to login.~n", [RobotId]),

  StateData = {RobotId, MessageDealer, {[], [logining]}},
  {ok, logining, StateData}.

logining({EventId, EventDetail, EventContent}, {RobotId, MessageDealer, {SoFar, [Next | Remaining]}}) ->
  case EventId of
    get_reply ->
      case EventDetail of
        logined ->
          lager:info("[Robot-~p] So far: ~p; Next: ~p; Remaining: ~p~n", [RobotId, SoFar, Next, Remaining]),
          lager:info("[Robot-~p] EventId: ~p; EventDetail: ~p; EventContent: ~p~n", [RobotId, EventId, EventDetail, EventContent]),
          {next_state, Next, {RobotId, MessageDealer, {[Next | SoFar], Remaining}}}
      end
  end.

handle_event(stop, _StateName, StateData) ->
  {stop, normal, StateData}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
  erlang:error(not_implemented).

handle_info(_Info, _StateName, _StateData) ->
  erlang:error(not_implemented).

terminate(Reason, StateName, StateData) ->
  RobotId = element(1, StateData),

  io:format("~p~n", [registered()]),

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  case whereis(Heartbeat) of
    undefine ->
      ok;
    HeartbeatPid ->
      lager:info("[HeartBeat ~p] pid=~p, stopping~n", [Heartbeat, HeartbeatPid]),
      Heartbeat ! stop
  end,

  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  case whereis(MessageDealer) of
    undefine ->
      ok;
    MessageDealerPid ->
      lager:info("[MessageDealer ~p] pid=~p, stopping~n", [MessageDealer, MessageDealerPid]),
      MessageDealer ! stop
  end,

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  case whereis(ReplyCallback) of
    undefine ->
      ok;
    ReplyCallbackPid ->
      lager:info("[RobotCallback ~p] pid=~p, stopping~n", [ReplyCallback, ReplyCallbackPid]),
      ReplyCallback ! stop
  end,

  io:format("~p~n", [registered()]),

  lager:info("robot fsm terminated, reason: ~p, state: ~p, data: ~p~n", [Reason, StateName, StateData]),

  robot_scheduler ! {start_robot, 1},
  ok.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
  erlang:error(not_implemented).
