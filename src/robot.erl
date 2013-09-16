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

  RobotFSM = list_to_atom("RobotFSM" ++ integer_to_list(RobotId)),
  RobotState = {[], [logining, moving]},
  RobotInfo = {500, 150, -1}, % 500 and 150 are the initial coordinates of a robot; the third element, -1, is the account id
  StateData = {RobotId, MessageDealer, RobotState, RobotInfo},
  gen_fsm:start_link({local, RobotFSM}, robot, StateData, []),

  loop(RobotFSM, RobotId),

  Heartbeat ! stop,
  MessageDealer ! stop.

loop(RobotFSM, RobotId) ->
  receive
    {received, ReplyBin} ->
      ReplyMsg = rpc_pb:decode_transunit(ReplyBin),
      case rpc_pb:get_extension(ReplyMsg, loginreply) of
        {ok, LoginReply} ->
          AccountId = (LoginReply#loginreply.accountinfo)#accountinfo.id,
          lager:info("Robot ~p logined.~n", [AccountId]),
          gen_fsm:send_event(RobotFSM, {get_reply, logined, AccountId});
        undefined ->
          lager:info("Robot undefined reply: ~p~n", [ReplyMsg])
      end
  after 60000 ->
    lager:error("robot ~p receiving timeout", [RobotId]),
    % TODO
    gen_fsm:send_event(RobotFSM, error)
  end,
  loop(RobotFSM, RobotId).

init(RobotState) ->
  RobotId = element(1, RobotState),
  TransUnit = rpc_req:login_req(RobotId),
%%   TransUnit = rpc_req:create_account_req(RobotId),
  MessageDealer = element(2, RobotState),
  MessageDealer ! {send, TransUnit},
  {ok, logining, RobotState}.

logining({get_reply, Event, EventInfo}, {RobotId, MessageDealer, {SoFar, RemainedState = [Next | Remaining]}, RobotInfo}) ->
  case Event of
    logined ->
      NewRobotInfo = moving_around(MessageDealer, setelement(3, RobotInfo, EventInfo)),
      {next_state, Next, {RobotId, MessageDealer, {[Next | SoFar], Remaining}, NewRobotInfo}};
    moving ->
      NewRobotInfo = moving_around(MessageDealer, RobotInfo),
      [CurrState | _] = SoFar,
      {next_state, CurrState, {RobotId, MessageDealer, {SoFar, RemainedState}, NewRobotInfo}}
  end.

moving_around(MessageDealer, RobotInfo) ->
  X = element(1, RobotInfo),
  Y = element(2, RobotInfo),
  AccountId = element(3, RobotInfo),
  TransUnit = rpc_req:move(X + random:uniform(50), Y + random:uniform(10), AccountId),
  lager:info("robot ~p is moving to (~p, ~p)~n", [AccountId, X, Y]),
  MessageDealer ! {send, TransUnit},
  {X, Y, AccountId}.

handle_event(_Event, _StateName, _StateData) ->
  erlang:error(not_implemented).

handle_sync_event(_Event, _From, _StateName, _StateData) ->
  erlang:error(not_implemented).

handle_info(_Info, _StateName, _StateData) ->
  erlang:error(not_implemented).

terminate(Reason, StateName, StateData) ->
  lager:error("robot fsm terminated, reason: ~p, state: ~p, data: ~p~n", [Reason, StateName, StateData]).

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
  erlang:error(not_implemented).
