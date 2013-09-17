%% Copyright
-module(robot).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-include("rpc_pb.hrl").

-behaviour(gen_fsm).

%% API
-export([init/2]).

%% gen_fsm callback
-export([init/1, logining/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

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
  RobotTimer = list_to_atom("robot_timer" ++ integer_to_list(RobotId)), % can only be created after robot has logged in

  RobotFSM = list_to_atom("RobotFSM" ++ integer_to_list(RobotId)),
  RobotState = {[], [logining]},
  StateData = {RobotId, MessageDealer, RobotState},
  gen_fsm:start_link({local, RobotFSM}, robot, StateData, []),

  loop(RobotFSM, RobotId, MessageDealer, RobotTimer),

  lager:info("[RobotTimer ~p] pid = ~p, stop~n", [RobotTimer, whereis(RobotTimer)]),
  RobotTimer ! stop,
  lager:info("[HeartBeat ~p] pid = ~p, stop~n", [Heartbeat, whereis(Heartbeat)]),
  Heartbeat ! stop,
  lager:info("[MessageDealer ~p] pid = ~p, stop~n", [MessageDealer, whereis(MessageDealer)]),
  MessageDealer ! stop.

loop(RobotFSM, RobotId, MessageDealer, RobotTimer) ->
  receive
    {received, ReplyBin} ->
      ReplyMsg = rpc_pb:decode_transunit(ReplyBin),
      case rpc_pb:get_extension(ReplyMsg, loginreply) of
        {ok, LoginReply} ->
          AccountId = (LoginReply#loginreply.accountinfo)#accountinfo.id,
          lager:info("[Robot ~p] Logined, accountId is ~p.~n", [RobotId, AccountId]),
          true = register(RobotTimer, spawn_link(robot_timer, start, [RobotId, AccountId, MessageDealer])),
          gen_fsm:send_event(RobotFSM, {get_reply, logined, AccountId});
        undefined -> ok
      end,
      case rpc_pb:get_extension(ReplyMsg, errormessage) of
        {ok, ErrorMessage} ->
          ErrorCode = ErrorMessage#errormessage.error,
          case ErrorCode of
            'NO_SUCH_ACCOUNT' ->
              CreateAccountReq = rpc_req:create_account_req(RobotId),
              MessageDealer ! {send, CreateAccountReq},
              lager:info("[Robot ~p] Send req: Create account.", [RobotId]);
            _ ->
              lager:warning("[Robot ~p] Received unknown ErrorMsg: ~p~n", [RobotId, ErrorCode])
          end;
        undefined ->
          lager:warning("[Robot ~p] Discard unknown reply: ~p~n", [RobotId, ReplyMsg])
      end,
      loop(RobotFSM, RobotId, MessageDealer, RobotTimer)
  after 120000 ->
    lager:error("[Robot ~p] Receiving timeout", [RobotId]),
    gen_fsm:send_all_state_event(RobotFSM, stop)
  end.

init(RobotState) ->
  RobotId = element(1, RobotState),
  TransUnit = rpc_req:login_req(RobotId),
  MessageDealer = element(2, RobotState),
  MessageDealer ! {send, TransUnit},
  lager:info("[Robot ~p] Send req: Login.~n", [RobotId]),
  {ok, logining, RobotState}.

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
  lager:info("robot fsm terminated, reason: ~p, state: ~p, data: ~p~n", [Reason, StateName, StateData]),
  ok.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
  erlang:error(not_implemented).
