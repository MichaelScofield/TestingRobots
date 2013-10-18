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

%% API
-export([start/0, init/0]).

%% LFC using gen_server behavior
init() ->
  lager:info("Starting robots scheduler."),
  true = register(robot_scheduler, spawn_link(?MODULE, start, [])),
  {ok, self()}.

start() ->
  process_flag(trap_exit, true),
  loop().

loop() ->
  receive
    {'EXIT', From, Reason} ->
      lager:warning("~p stopped, reason: ~p~n", [From, Reason]),
      start_robot(1);
    {start_robot, N} ->
      lager:info("Ready to start ~p robots.", [N]),
      start_robot(N)
  end,
  loop().

start_robot(0) ->
  ok;
start_robot(N) ->
  {ReadyRobotIds, RunningRobotIds} = gen_server:call(robots_global, {get, all_robot_ids}),
  Index = gen_server:call(robots_global, {get, next_random, length(ReadyRobotIds)}),
  if
    Index == 0 ->
      lager:warning("No robot can be started."),
      ok;
    true ->
      RobotId = lists:nth(Index, ReadyRobotIds),
      spawn_link(robot, start_link, [RobotId]),
      NewReadyRobotIds = lists:delete(RobotId, ReadyRobotIds),
      ok = gen_server:call(robots_global, {set, all_robot_ids, {NewReadyRobotIds, [RobotId | RunningRobotIds]}}),
      start_robot(N - 1)
  end.
