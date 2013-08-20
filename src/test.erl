%% Copyright
-module(test).
-author("SKS").

%% API
-export([test/0]).

-include("rpc_pb.hrl").

test() ->
  LoginReqBin = rpc_pb:encode_loginrequest(#loginrequest{device_id = "1", client_version = "2", meta_crc32 = "3"}),
  io:format("LoginReq Binary: ~w~n", [LoginReqBin]),
  LoginReqMsg = rpc_pb:decode_loginrequest(list_to_binary(LoginReqBin)),
  io:format("LoginReq Message: ~w~n", [LoginReqMsg]),
  {ok, TransUnitMsg} = rpc_pb:set_extension(#transunit{sn = 1, '$extensions' = dict:new()}, msg, LoginReqMsg),
  io:format("TransUnit Message: ~w~n", [TransUnitMsg]),
  TransUnitBin = rpc_pb:encode_transunit(TransUnitMsg),
  io:format("TransUnit Binary: ~w~n", [TransUnitBin]),
  TransUnitDecoded = rpc_pb:decode_transunit(list_to_binary(TransUnitBin)),
  io:format("TransUnit decoded: ~w~n", [TransUnitDecoded]),
  {ok, Extension} = rpc_pb:get_extension(TransUnitDecoded, msg),
  io:format("Extensions: ~w~n", [Extension]).