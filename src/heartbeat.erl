%% Copyright
-module(heartbeat).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([pow/2]).

pow(RobotId, MessageDealer) ->
  receive
    stop ->
      lager:debug("[Robot-~p] Stop heartbeating.~n", [RobotId]),
      stop
  after 30000 ->
    MessageDealer ! {send, rpc_req:ping()},
    lager:info("[Robot-~p] Ping.~n", [RobotId]),
    pow(RobotId, MessageDealer)
  end.