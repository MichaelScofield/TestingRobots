%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-include("rpc_pb.hrl").

-behaviour(gen_fsm).

%% gen_fsm callback
-export([init/1, logining/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

init(RobotId) ->
  lager:info("create robot id = " ++ integer_to_list(RobotId)),

  SupervisorId = list_to_atom("robot-supervisor-" ++ integer_to_list(RobotId)),

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  ReplyCallbackSpec = {ReplyCallback, {robot_callback, start, [RobotId]}, permanent, brutal_kill, worker, [robot_callback]},
  supervisor:start_child(SupervisorId, ReplyCallbackSpec),

  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  MessageDealerSpec = {MessageDealer, {message_dealer, start, [RobotId, whereis(ReplyCallback)]}, permanent, brutal_kill, worker, [message_dealer]},
  supervisor:start_child(SupervisorId, MessageDealerSpec),

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  HeartbeatSpec = {Heartbeat, {heartbeat, start, [RobotId, MessageDealer]}, permanent, brutal_kill, worker, [heartbeat]},
  supervisor:start_child(SupervisorId, HeartbeatSpec),

  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer ! {send, TransUnit},
  lager:info("[Robot ~p] Send req: Login.~n", [RobotId]),

  StateData = {RobotId, MessageDealer, {[], [logining]}},
  {ok, logining, StateData}.

logining({EventId, EventDetail, EventContent}, {RobotId, MessageDealer, {SoFar, [Next | Remaining]}}) ->
  case EventId of
    get_reply ->
      case EventDetail of
        logined ->
          lager:debug("[Robot FSM ~p] So far: ~p; Next: ~p; Remaining: ~p~n", [RobotId, SoFar, Next, Remaining]),
          lager:debug("[Robot FSM ~p] EventId: ~p; EventDetail: ~p; EventContent: ~p~n", [RobotId, EventId, EventDetail, EventContent]),
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

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  lager:info("[HeartBeat ~p] pid = ~p, stop~n", [Heartbeat, whereis(Heartbeat)]),
  Heartbeat ! stop,

  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  lager:info("[MessageDealer ~p] pid = ~p, stop~n", [MessageDealer, whereis(MessageDealer)]),
  MessageDealer ! stop,

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  lager:info("[RobotReplyCallback ~p] pid = ~p, stop~n", [ReplyCallback, whereis(ReplyCallback)]),
  ReplyCallback ! stop,

  lager:info("robot fsm terminated, reason: ~p, state: ~p, data: ~p~n", [Reason, StateName, StateData]),
  ok.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
  erlang:error(not_implemented).
