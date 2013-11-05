%% Copyright
-module(message_dealer).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-include("rpc_pb.hrl").

%% API
-export([start/2]).

start(RobotId, RobotPid) ->
  {ok, Context} = erlzmq:context(),
  {ok, Socket} = erlzmq:socket(Context, dealer),
  ok = erlzmq:setsockopt(Socket, identity, pid_to_list(self())),
  ServerAddr = gen_server:call(robot_status, {get, server_addr}),
  ok = erlzmq:connect(Socket, ServerAddr),
  loop(RobotId, RobotPid, Socket, Context).

loop(RobotId, RobotPid, Socket, Context) ->
  receive
    stop ->
      list_to_atom("robot-timer-" ++ integer_to_list(RobotId)) ! stop,

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
      loop(RobotId, RobotPid, Socket, Context)
  after
    100 ->
      case polling(Socket, 100, 10) of
        {error, eterm} ->
          stop;
        {error, timeout} ->
          loop(RobotId, RobotPid, Socket, Context);
        {ok, Msg} ->
          handle_reply(RobotId, RobotPid, Msg),
          loop(RobotId, RobotPid, Socket, Context)
      end
  end.

handle_reply(RobotId, RobotPid, ReplyBin) ->
  ReplyMsg = case catch rpc_pb:decode_transunit(ReplyBin) of
               {error, Error} ->
                 lager:error("[Robot-~p] Cannot decode reply ~p.~n", [RobotId, ReplyBin]),
                 exit(Error);
               Reply ->
                 Reply
             end,
  case rpc_pb:get_extension(ReplyMsg, loginreply) of
    {ok, LoginReply} ->
      AccountId = (LoginReply#loginreply.accountinfo)#accountinfo.id,
      true = register(list_to_atom("robot-timer-" ++ integer_to_list(RobotId)), spawn_link(robot_timer, start, [RobotId, AccountId, self()])),
      RobotPid ! {logined, AccountId};
    undefined -> ok
  end,
  case rpc_pb:get_extension(ReplyMsg, errormessage) of
    {ok, ErrorMessage} ->
      ErrorCode = ErrorMessage#errormessage.error,
      case ErrorCode of
        'NO_SUCH_ACCOUNT' ->
          CreateAccountReq = rpc_req:create_account_req(RobotId),
          self() ! {send, CreateAccountReq},
          lager:info("[Robot-~p] Creating account.", [RobotId]);
        'CLIENT_DISCONNECT' ->
          exit(ErrorCode);
        _ ->
          lager:warning("[Robot-~p] Received unknown ErrorMsg: ~p~n", [RobotId, ErrorCode])
      end;
    undefined ->
      lager:warning("[Robot-~p] Discard unknown reply: ~p~n", [RobotId, ReplyMsg])
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