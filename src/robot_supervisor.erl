%% Copyright
-module(robot_supervisor).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callback
-export([init/1]).

start_link(RobotId) ->
  lager:start(),
  SupervisorId = list_to_atom("robot-supervisor-" ++ integer_to_list(RobotId)),
  supervisor:start_link({local, SupervisorId}, ?MODULE, RobotId).

init(RobotId) ->
  SupervisorSpec = {one_for_all, 1, 60},

  ChildId = list_to_atom("robot-" ++ integer_to_list(RobotId)),
  ChildSpec = {ChildId, {robot, start_link, [RobotId]}, permanent, brutal_kill, worker, [robot]},

  {ok, {SupervisorSpec, [ChildSpec]}}.
