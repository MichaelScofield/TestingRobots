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
  lager:info("Starting robots scheduler."),
  {ok, {ReadyRobotIds, []}}.

handle_call({start_robot, N}, _From, State) ->
  lager:info("Ready to start ~p robots.", [N]),
  {ok, NewState} = start_robot(State, N),
  {reply, ok, NewState}.

handle_info({'EXIT', From, Reason}, State) ->
  lager:warning("Robot ~p stopped, reason: ~p~n", [From, Reason]),
  {ok, NewState} = start_robot(State, 1),
  {noreply, NewState}.

start_robot(State, 0) ->
  {ok, State};
start_robot({ReadyRobotIds, RunningRobotIds} = State, N) ->
  Index = gen_server:call(robots_global, {get, next_random, length(ReadyRobotIds)}),
  if
    Index == 0 ->
      lager:warning("No robot can be started."),
      {ok, State};
    true ->
      RobotId = lists:nth(Index, ReadyRobotIds),
      Pid = spawn(robot, start_link, [RobotId]),
      lager:info("ReadyRobotIds=~w, starting Robot ~p(~p)~n", [ReadyRobotIds, RobotId, Pid]),
      NewReadyRobotIds = lists:delete(RobotId, ReadyRobotIds),
      start_robot({NewReadyRobotIds, [RobotId | RunningRobotIds]}, N - 1)
  end.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  lager:warning("Robot Scheduler terminated, reason:~p, state:~p~n", [Reason, State]),
  ok.

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
