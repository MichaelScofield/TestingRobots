%% Copyright
-module(robot_timer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/3]).

%% Created in robot_callback:loop/4.
start(RobotId, AccountId, MessageDealer) ->
  lager:info("create robot timer id ~p~n", [RobotId]),
  RobotMovingState = {400, 100, AccountId}, % 400 and 100 are the initial coordinates of a robot
  random:seed(), % Generate default random seed.
  tick(moving, RobotId, MessageDealer, 10000, RobotMovingState).

tick(moving, RobotId, MessageDealer, Timeout, {X, Y, AccountId}) ->
  {RandomX, _} = random:uniform_s(50, random:seed(now())),
  NewX = X + RandomX,
  {RandomY, _} = random:uniform_s(10, random:seed(now())),
  NewY = Y + RandomY,
  MessageDealer ! {send, rpc_req:move(NewX, NewY, AccountId)},
  lager:info("[Robot ~p] Send req: Moving to (~p, ~p)~n", [RobotId, X, Y]),
  receive
    stop ->
      lager:warning("stop robot timer ~p~n", [RobotId]),
      stop
  after Timeout ->
    tick(moving, RobotId, MessageDealer, Timeout, {NewX, NewY, AccountId})
  end.