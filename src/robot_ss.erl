%% Copyright
-module(robot_ss).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor callback
-export([init/1]).

-define(
CHILD_SPEC(Name, RobotId),
  {Name, {robot_supervisor, start_link, [RobotId]}, transient, infinity, supervisor, [robot_supervisor]}
).

start_link(RobotStartId, RobotCount)
  when is_integer(RobotStartId), is_integer(RobotCount), RobotStartId > 0, RobotCount > 0 ->
  lager:start(),
  supervisor:start_link({local, robot_super_supervisor}, ?MODULE, {RobotStartId, RobotCount}),

  supervisor:start_child(robot_super_supervisor, {robots_state_server, {robots_global, start_link, []}, permanent, brutal_kill, worker, [robots_global]}),

  RobotsList = lists:seq(RobotStartId, RobotStartId + RobotCount - 1),
  start_robot(RobotsList).

init({RobotStartId, RobotCount}) ->
  SupervisorSpec = {one_for_one, 1, 60},
  ChildrenSpec = [?CHILD_SPEC(Name, RobotId) || {Name, RobotId} <- generate_child_spec(RobotStartId, RobotCount)],
  {ok, {SupervisorSpec, ChildrenSpec}}.

start_robot([]) ->
  ok;
start_robot([RobotId | RobotIdList]) ->
  RobotFSMId = list_to_atom("robot-fsm-" ++ integer_to_list(RobotId)),
  gen_fsm:send_event(RobotFSMId, res),
  start_robot(RobotIdList).

generate_child_spec(RobotStartId, RobotCount) ->
  generate_child_spec(RobotStartId, RobotCount, 0, []).
generate_child_spec(RobotStartId, RobotCount, N, Acc) when N < RobotCount ->
  RobotId = RobotStartId + N,
  ChildName = list_to_atom("supervisor-child-" ++ integer_to_list(RobotId)),
  generate_child_spec(RobotStartId, RobotCount, N + 1, [{ChildName, RobotId} | Acc]);
generate_child_spec(_, _, _, Acc) ->
  lists:reverse(Acc).