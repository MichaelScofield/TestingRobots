%% Copyright
-module(message_dealer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start/2, loop/4]).

start(RobotId, Receiver) ->
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, dealer),
  ok = erlzmq:setsockopt(Socket, identity, pid_to_list(self())),
%%   ServerAddr = "tcp://10.10.10.10:5570",
%%   ServerAddr = "tcp://10.10.9.116:5570",
  ServerAddr = "tcp://127.0.0.1:5570",
  ok = erlzmq:connect(Socket, ServerAddr),
  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  lager:info("[Robot-~p] Robot start dealing with messages. (~p)~n", [RobotId, MessageDealer]),
  loop(RobotId, Receiver, Socket, Context).

loop(RobotId, Receiver, Socket, Context) ->
  receive
    stop ->
      erlzmq:close(Socket),
      erlzmq:term(Context),
      lager:warning("[Robot-~p] Robot stop dealing with messages.~n", [RobotId]),
      stop;
    {send, TransUnit} ->
      Bin = list_to_binary(rpc_pb:encode_transunit(TransUnit)),
      erlzmq:send(Socket, Bin)
  after
    100 ->
      case polling(Socket, 100, 10) of
        {error, _} ->
          ok;
        {ok, Msg} ->
          Receiver ! {received, Msg}
      end
  end,
  loop(RobotId, Receiver, Socket, Context).

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