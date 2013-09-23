%% Copyright
-module(robot_ss).
-author("lfc").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor callback
-export([init/1]).

-define(
CHILD_SPEC(Name, RobotId),
  {Name, {robot_supervisor, start_link, [RobotId]}, transient, infinity, [robot_supervisor]}
).

start_link(RobotStartId, RobotCount)
  when is_integer(RobotStartId), is_integer(RobotCount), RobotStartId > 0, RobotCount > 0 ->
  lager:start(),
  supervisor:start_link({local, robot_super_supervisor}, ?MODULE, {RobotStartId, RobotCount}).

init({RobotStartId, RobotCount}) ->
  SupervisorSpec = {one_for_one, 1, 60},
  ChildrenSpec = [?CHILD_SPEC(Name, RobotId) || {Name, RobotId} <- generate_child_spec(RobotStartId, RobotCount)],
  {ok, SupervisorSpec, ChildrenSpec}.

generate_child_spec(RobotStartId, RobotCount) ->
  generate_child_spec(RobotStartId, RobotCount, 0, []).
generate_child_spec(RobotStartId, RobotCount, N, Acc) when N < RobotCount ->
  RobotId = RobotStartId + N,
  ChildName = list_to_atom("supervisor-child-" ++ integer_to_list(RobotId)),
  generate_child_spec(RobotStartId, RobotCount, N + 1, [{ChildName, RobotId} | Acc]);
generate_child_spec(_, _, _, Acc) ->
  lists:reverse(Acc).