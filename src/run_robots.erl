%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Oct 2013 5:20 PM
%%%-------------------------------------------------------------------
-module(run_robots).
-author("lfc").

-behaviour(application).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2, stop/1, start/0]).

%% erl -detached
%%     -server_addr "tcp://10.10.10.10:5570"
%%     -robot_start_id RobotStartId(int)
%%     -robot_count RobotCount(int)
%%     -running_robots_count RunningRobotsCount(int)
%%     -robot_type RobotType(atom)
%%     -s run_robots start
start() ->
  application:start(?MODULE).

start(normal, _StartArgs) ->
  {ok, [[ServerAddr]]} = init:get_argument(server_addr),

  {ok, [[RobotStartId]]} = init:get_argument(robot_start_id),
  {ok, [[RobotCount]]} = init:get_argument(robot_count),
  {ok, [[RunningRobotsCount]]} = init:get_argument(running_robots_count),

  case init:get_argument(robot_type) of
    error ->
      robot_ss:start_link(list_to_integer(RobotStartId), list_to_integer(RobotCount), list_to_integer(RunningRobotsCount), ServerAddr, idle);
    {ok, [[RobotType]]} ->
      robot_ss:start_link(list_to_integer(RobotStartId), list_to_integer(RobotCount), list_to_integer(RunningRobotsCount), ServerAddr, list_to_atom(RobotType))
  end.

stop(_State) ->
  ok.