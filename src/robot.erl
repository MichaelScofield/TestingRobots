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
  ServerAddr = "tcp://10.10.10.10:5570",
%%   ServerAddr = "tcp://10.10.9.116:5570",
%%   ServerAddr = "tcp://127.0.0.1:5570",
  ok = erlzmq:connect(Socket, ServerAddr),
  io:format("connect to ~s success.~n", [ServerAddr]),
%%   TransUnit = login_req(),
  TransUnit = create_account_req(),
  send(Socket, TransUnit),
  close(Socket),
  terminate(Context).

create_account_req() ->
  Message = #createavatarrequest{device_id = "Erobot1", name = "Robot1", meta_id = 131011},
  wrap_transunit(createavatarrequest, Message).

login_req() ->
  Message = #loginrequest{device_id = "TestDeviceId1", client_version = "0.5.0", meta_crc32 = "8414FF96"},
  wrap_transunit(loginrequest, Message).

wrap_transunit(Type, Message) when is_atom(Type) ->
  {ok, TransUnit} = rpc_pb:set_extension(#transunit{sn = 1, '$extensions' = dict:new()}, Type, Message),
  TransUnit.

send(Socket, TransUnit) ->
  Bin = list_to_binary(rpc_pb:encode_transunit(TransUnit)),
  erlzmq:send(Socket, Bin),
  loop(Socket, 3).

loop(_Socket, 0) ->
  over;
loop(Socket, N) when N > 0 ->
  case polling(Socket, 100, 10) of
    {error, timeout} ->
      io:format("Error: Received timeout.~n"),
      loop(Socket, N - 1);
    {ok, ReplyBin} ->
      TransUnit = rpc_pb:decode_transunit(ReplyBin),
      io:format("ReplyMsg = ~w~n", [TransUnit]),
      loop(Socket, N)
  end.

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