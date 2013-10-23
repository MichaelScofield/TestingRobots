%% Copyright
-module(message_dealer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/1, loop/4]).

start(RobotId) ->
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, dealer),
  ok = erlzmq:setsockopt(Socket, identity, pid_to_list(self())),
  ServerAddr = "tcp://10.10.10.10:5570",
%%   ServerAddr = "tcp://10.10.9.116:5570",
%%   ServerAddr = "tcp://127.0.0.1:5570",
  ok = erlzmq:connect(Socket, ServerAddr),
  lager:info("[Robot-~p] Start dealing with messages.(~p)~n", [RobotId, self()]),
  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  loop(RobotId, ReplyCallback, Socket, Context).

loop(RobotId, Receiver, Socket, Context) ->
  receive
    stop ->
      erlzmq:close(Socket),
      erlzmq:term(Context),
      lager:warning("[Robot-~p] Stop dealing with messages.~n", [RobotId]),
      stop;
    {send, TransUnit} ->
      Bin = list_to_binary(rpc_pb:encode_transunit(TransUnit)),
      case erlzmq:send(Socket, Bin) of
        ok ->
          ok;
        Error ->
          lager:error("[Robot-~p] Msg ~p cannot be sent. Error: ~p.~n", [RobotId, TransUnit, Error])
      end,
      loop(RobotId, Receiver, Socket, Context)
  after
    100 ->
      case polling(Socket, 100, 10) of
        {error, eterm} ->
          stop;
        {error, timeout} ->
          loop(RobotId, Receiver, Socket, Context);
        {ok, Msg} ->
          Receiver ! {received, Msg},
          loop(RobotId, Receiver, Socket, Context)
      end
  end.

polling(_Socket, 0, _Delay) ->
  {error, timeout};
polling(Socket, N, Delay) when N > 0 ->
  case erlzmq:recv(Socket, [dontwait]) of
    {ok, Msg} ->
      {ok, Msg};
    {error, eagain} ->
      timer:sleep(Delay),
      polling(Socket, N - 1, Delay);
    {error, eterm} ->
      {error, eterm}
  end.