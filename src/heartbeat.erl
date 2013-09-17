%% Copyright
-module(heartbeat).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2]).

start(RobotId, MessageDealer) ->
  lager:info("create heartbeat id ~p~n", [RobotId]),
  pow(RobotId, MessageDealer).

pow(RobotId, MessageDealer) ->
  MessageDealer ! {send, rpc_req:ping()},
  lager:info("[Robot ~p] Send req: Ping.~n", [RobotId]),
  receive
    stop ->
      lager:warning("stop heartbeat ~p~n", [RobotId]),
      stop
  after 30000 ->
    pow(RobotId, MessageDealer)
  end.