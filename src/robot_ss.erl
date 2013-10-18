%% Copyright
-module(robot_ss).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% supervisor callback
-export([init/1]).

start_link(RobotStartId, RobotCount, RunningRobotsCount) ->
  lager:start(),
  supervisor:start_link({local, robot_super_supervisor}, ?MODULE, []),

  ReadyRobotIds = lists:seq(RobotStartId, RobotStartId + RobotCount - 1),
  ok = gen_server:call(robots_global, {set, all_robot_ids, {ReadyRobotIds, []}}),

  robot_scheduler ! {start_robot, RunningRobotsCount},
  ok.

init(_Args) ->
  SupervisorSpec = {one_for_one, 1, 60},
  RobotStateServerSpec = {robot_state_server, {robots_global, start_link, []}, permanent, brutal_kill, worker, [robots_global]},
  RobotSchedulerSpec = {robot_scheduler_process, {robot_scheduler, init, []}, permanent, brutal_kill, worker, [robot_scheduler]},
  ChildrenSpec = [RobotStateServerSpec, RobotSchedulerSpec],
  {ok, {SupervisorSpec, ChildrenSpec}}.
