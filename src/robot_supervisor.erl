%% Copyright
-module(robot_supervisor).
-author("lfc").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callback
-export([init/1]).

start_link(RobotId) ->
  SupervisorId = list_to_atom(atom_to_list(?MODULE) ++ "-" ++ integer_to_list(RobotId)),
  supervisor:start_link({local, SupervisorId}, ?MODULE, RobotId).

init(RobotId) ->
  SupervisorSpec = {one_for_all, 1, 60},

  ChildId = list_to_atom("robot-" ++ integer_to_list(RobotId)),
  RobotFSMId = list_to_atom("robot-fsm-" ++ integer_to_list(RobotId)),
  ChildSpec = {ChildId, {gen_fsm, start_link, [{local, RobotFSMId}, robot, RobotId, []]}, permanent, brutal_kill, worker, [robot]},

  {ok, SupervisorSpec, ChildSpec}.
