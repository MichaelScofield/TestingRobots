%% Copyright
-module(robot).
-author("lfc").

-include("rpc_pb.hrl").

%% API
-export([start/0]).

start() ->
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, dealer),
  ok = erlzmq:setsockopt(Socket, identity, pid_to_list(self())),
%%   ServerAddr = "tcp://10.10.9.164:5570",
  ServerAddr = "tcp://127.0.0.1:5570",
  ok = erlzmq:connect(Socket, ServerAddr),
  io:format("connect to ~s success.~n", [ServerAddr]),
  login(Socket),
  close(Socket),
  terminate(Context).

login(Socket) ->
  LoginReq = #loginrequest{device_id = "TestDeviceId1", client_version = "0.5.0", meta_crc32 = "F7B16A40"},
  {ok, TransUnit} = rpc_pb:set_extension(#transunit{sn = 1, '$extensions' = dict:new()}, msg, LoginReq),
%%   io:format("TransUnit: ~w~n", [TransUnit]),
  Msg = list_to_binary(rpc_pb:encode_transunit(TransUnit)),
  erlzmq:send(Socket, Msg),
  loop(Socket).

loop(Socket) ->
  case polling(Socket, 100, 10) of
    {error, timeout} ->
      io:format("Error: Received timeout.~n");
    {ok, ReplyBin} ->
      #transunit{'$extensions' = Extensions} = rpc_pb:decode_transunit(ReplyBin),
      io:format("ReplyMsg: Extensions= ~w~n", [Extensions])
  end,
  loop(Socket).

polling(_Socket, 0, _Delay) ->
  {error, timeout};
polling(Socket, N, Delay) when N > 0 ->
  case erlzmq:recv(Socket, [dontwait]) of
    {ok, Msg} ->
      {ok, Msg};
    {error, eagain} ->
      timer:sleep(Delay),
      polling(Socket, N - 1, Delay)
  end.

close(Socket) ->
  erlzmq:close(Socket).

terminate(Context) ->
  erlzmq:term(Context).