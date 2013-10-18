%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callback
-export([init/1, logining/2, prepare/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

start_link(RobotId) ->
  RobotFSMId = list_to_atom("robot-fsm-" ++ integer_to_list(RobotId)),
  gen_fsm:start_link({local, RobotFSMId}, robot, RobotId, []).

init(RobotId) ->
  lager:info("[Robot-~p] Robot FSM created.~n", [RobotId]),
  {ok, prepare, RobotId}.

prepare(res, RobotId) ->
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
  lager:info("[Robot-~p] Trying to login.~n", [RobotId]),

  StateData = {RobotId, MessageDealer, {[], [logining]}},
  {next_state, logining, StateData}.

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
  SupervisorId = list_to_atom("robot-supervisor-" ++ integer_to_list(RobotId)),

  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
%%   lager:info("[HeartBeat ~p] pid=~p, stopping~n", [Heartbeat, whereis(Heartbeat)]),
%%   Heartbeat ! stop,
  supervisor:terminate_child(SupervisorId, Heartbeat),

  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
%%   lager:info("[MessageDealer ~p] pid=~p, stopping~n", [MessageDealer, whereis(MessageDealer)]),
%%   MessageDealer ! stop,
  supervisor:terminate_child(SupervisorId, MessageDealer),

  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
%%   lager:info("[RobotCallback ~p] pid=~p, stopping~n", [ReplyCallback, whereis(ReplyCallback)]),
%%   ReplyCallback ! stop,
  supervisor:terminate_child(SupervisorId, ReplyCallback),

  lager:info("robot fsm terminated, reason: ~p, state: ~p, data: ~p~n", [Reason, StateName, StateData]),
  ok.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
  erlang:error(not_implemented).
