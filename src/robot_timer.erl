%% Copyright
-module(robot_timer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/3]).

%% Created in robot_callback:loop/4.
start(RobotId, AccountId, MessageDealer) ->
  RobotMovingState = {1250, 50, AccountId},
  Timeout = infinity, %% NOTE one time ticker
%%   Timeout = 10000,
  lager:info("[Robot-~p] Robot timer created.~n", [RobotId]),
  tick(moving, RobotId, MessageDealer, Timeout, RobotMovingState).

tick(moving, RobotId, MessageDealer, Timeout, {X, Y, AccountId}) ->
  NewX = X + gen_server:call(robots_global, {get, next_random, 1250}),
  NewY = Y + gen_server:call(robots_global, {get, next_random, 100}),
  MessageDealer ! {send, rpc_req:move(NewX, NewY, AccountId)},
  lager:info("[Robot-~p] Moving to (~p,~p)~n", [RobotId, NewX, NewY]),
  receive
    stop ->
      lager:warning("[Robot-~p] RobotTimer received stop message.", [RobotId]),
      stop
  after Timeout ->
    tick(moving, RobotId, MessageDealer, Timeout, {NewX, NewY, AccountId})
  end.