%% Copyright
-module(robot_timer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/3]).

%% Created in robot_callback:loop/4.
start(RobotId, AccountId, MessageDealer) ->
  lager:info("create robot timer id ~p~n", [RobotId]),
  RobotMovingState = {1250, 50, AccountId},
  random:seed(now()), % Generate default random seed.
  Timeout = infinity, %% NOTE one time ticker
%%   Timeout = 10000,
  tick(moving, RobotId, MessageDealer, Timeout, RobotMovingState).

tick(moving, RobotId, MessageDealer, Timeout, {X, Y, AccountId}) ->
  NewX = X + gen_server:call(robots_global, {get, next_random, 1250}),
  NewY = Y + gen_server:call(robots_global, {get, next_random, 100}),
  MessageDealer ! {send, rpc_req:move(NewX, NewY, AccountId)},
  lager:info("[Robot ~p] Send req: Moving to (~p, ~p)~n", [RobotId, NewX, NewY]),
  receive
    stop ->
      lager:warning("stop robot timer ~p~n", [RobotId]),
      stop
  after Timeout ->
    tick(moving, RobotId, MessageDealer, Timeout, {NewX, NewY, AccountId})
  end.