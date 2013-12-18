%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Oct 2013 4:20 PM
%%%-------------------------------------------------------------------
-module(robot_scheduler).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

start_link(ReadyRobotIds) ->
  gen_server:start_link({local, robot_scheduler}, ?MODULE, ReadyRobotIds, []).

init(ReadyRobotIds) ->
  {ok, {ReadyRobotIds, []}}.

handle_cast({start_robot, N}, State) ->
  {ok, NewState} = start_robot(State, N),
  {noreply, NewState};
handle_cast({return_robot, RobotId}, {ReadyRobotIds, RunningRobotIds}) ->
  lager:info("[RobotScheduler] Stopping robot ~p", [RobotId]),
  case lists:keyfind(RobotId, 1, RunningRobotIds) of
    false ->
      {noreply, {ReadyRobotIds, RunningRobotIds}};
    {RobotId, RobotType} ->
      NewRunningRobotIds = lists:keydelete(RobotId, 1, RunningRobotIds),
      NewReadyRobotIds = lists:keystore(RobotId, 1, ReadyRobotIds, {RobotId, RobotType}),
      {noreply, {NewReadyRobotIds, NewRunningRobotIds}}
  end.

handle_info({'EXIT', From, Reason}, State) ->
  lager:warning("[RobotScheduler] Robot ~p stopped, reason: ~p~n", [From, Reason]),
  {ok, NewState} = start_robot(State, 1),
  {noreply, NewState}.

start_robot(State, 0) ->
  {ok, State};
start_robot({ReadyRobotIds, RunningRobotIds} = State, N) ->
  Index = gen_server:call(robot_status, {get, next_random, length(ReadyRobotIds)}),
  if
    Index == 0 ->
      lager:warning("[RobotScheduler] No robot can be started."),
      {ok, State};
    true ->
      {RobotId, RobotType} = lists:nth(Index, ReadyRobotIds),
      RobotProc = spawn(robot, start, [RobotId, RobotType]),
      lager:info("[RobotScheduler] ReadyRobotIds:~w. Starting robot ~p (~p)~n", [ReadyRobotIds, RobotId, RobotProc]),
      NewReadyRobotIds = lists:keydelete(RobotId, 1, ReadyRobotIds),
      NewRunningRobotIds = lists:keystore(RobotId, 1, RunningRobotIds, {RobotId, RobotType}),
      start_robot({NewReadyRobotIds, NewRunningRobotIds}, N - 1)
  end.

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  lager:warning("Robot Scheduler terminated, reason:~p, state:~p~n", [Reason, State]),
  ok.

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
