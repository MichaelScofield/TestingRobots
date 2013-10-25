%% Copyright
-module(heartbeat).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2, pow/2]).

start(RobotId, MessageDealer) ->
  lager:info("[Robot-~p] Start heartbeating.~n", [RobotId]),
  pow(RobotId, MessageDealer).

pow(RobotId, MessageDealer) ->
  receive
    stop ->
      lager:warning("[Robot-~p] Stop heartbeating.~n", [RobotId]),
      stop
  after 30000 ->
    MessageDealer ! {send, rpc_req:ping()},
    lager:info("[Robot-~p] Ping.~n", [RobotId]),
    pow(RobotId, MessageDealer)
  end.