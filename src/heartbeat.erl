%% Copyright
-module(heartbeat).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2, pow/2]).

start(RobotId, MessageDealer) ->
  Heartbeat = list_to_atom("robot-hb-" ++ integer_to_list(RobotId)),
  lager:info("[Robot-~p] Robot start heartbeating. (~p)~n", [RobotId, Heartbeat]),
  pow(RobotId, MessageDealer).

pow(RobotId, MessageDealer) ->
  receive
    stop ->
      lager:warning("[Robot-~p] Robot stop heartbeating.~n", [RobotId]),
      stop
  after 30000 ->
    MessageDealer ! {send, rpc_req:ping()},
    lager:info("[Robot-~p] Ping.~n", [RobotId]),
    pow(RobotId, MessageDealer)
  end.