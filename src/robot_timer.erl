%% Copyright
-module(robot_timer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/3]).

%% Created in robot_callback:loop/4.
start(RobotId, AccountId, MessageDealer) ->
  RobotMovingState = {500, 50, AccountId},
  Timeout = infinity, %% NOTE one time ticker
%%   Timeout = 10000,
  tick(moving, RobotId, MessageDealer, Timeout, RobotMovingState).

tick(moving, RobotId, MessageDealer, Timeout, {X, Y, AccountId}) ->
  NewX = X + gen_server:call(robot_status, {get, next_random, 1800}),
  NewY = Y + gen_server:call(robot_status, {get, next_random, 100}),
  MessageDealer ! {send, rpc_req:move(NewX, NewY, AccountId)},
%%   lager:info("[Robot-~p] Moving to (~p,~p)~n", [RobotId, NewX, NewY]),
  receive
    stop ->
      lager:debug("[Robot-~p] Timer stop ticking.", [RobotId]),
      stop
  after Timeout ->
    tick(moving, RobotId, MessageDealer, Timeout, {NewX, NewY, AccountId})
  end.