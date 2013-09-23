%% Copyright
-module(robot_timer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/3]).

start(RobotId, AccountId, MessageDealer) ->
  lager:info("create robot timer id ~p~n", [RobotId]),
  RobotMovingState = {500, 150, AccountId},
  tick(moving, RobotId, MessageDealer, 30000, RobotMovingState). % 500 and 150 are the initial coordinates of a robot

tick(moving, RobotId, MessageDealer, Timeout, {X, Y, AccountId}) ->
  NewX = X + random:uniform(50),
  NewY = Y + random:uniform(10),
  MessageDealer ! {send, rpc_req:move(NewX, NewY, AccountId)},
  lager:info("[Robot ~p] Send req: Moving to (~p, ~p)~n", [AccountId, X, Y]),
  receive
    stop ->
      lager:warning("stop robot timer ~p~n", [RobotId]),
      stop
  after Timeout ->
    tick(moving, RobotId, MessageDealer, Timeout, {NewX, NewY, AccountId})
  end.