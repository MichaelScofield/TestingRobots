%% Copyright
-module(robot_callback).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

-include("rpc_pb.hrl").

%% API
-export([start/1, loop/4]).

start(RobotId) ->
  RobotFSMId = list_to_atom("robot-fsm-" ++ integer_to_list(RobotId)),
  MessageDealer = list_to_atom("robot-md-" ++ integer_to_list(RobotId)),
  RobotTimer = list_to_atom("robot-timer-" ++ integer_to_list(RobotId)), % can only be created after robot has logged in
  ReplyCallback = list_to_atom("robot-cb-" ++ integer_to_list(RobotId)),
  register(ReplyCallback, spawn_link(?MODULE, loop, [RobotFSMId, RobotId, MessageDealer, RobotTimer])),
  lager:info("[Robot-~p] Robot callback created. (~p)~n", [RobotId, ReplyCallback]).

loop(RobotFSMId, RobotId, MessageDealer, RobotTimer) ->
  receive
    {received, ReplyBin} ->
      ReplyMsg = rpc_pb:decode_transunit(ReplyBin),
      case rpc_pb:get_extension(ReplyMsg, loginreply) of
        {ok, LoginReply} ->
          AccountId = (LoginReply#loginreply.accountinfo)#accountinfo.id,
          lager:info("[Robot-~p] Login ok, accountId=~p.~n", [RobotId, AccountId]),
          true = register(RobotTimer, spawn_link(robot_timer, start, [RobotId, AccountId, MessageDealer])),
          gen_fsm:send_event(RobotFSMId, {get_reply, logined, AccountId});
        undefined -> ok
      end,
      case rpc_pb:get_extension(ReplyMsg, errormessage) of
        {ok, ErrorMessage} ->
          ErrorCode = ErrorMessage#errormessage.error,
          case ErrorCode of
            'NO_SUCH_ACCOUNT' ->
              CreateAccountReq = rpc_req:create_account_req(RobotId),
              MessageDealer ! {send, CreateAccountReq},
              lager:info("[Robot-~p] Creating account.", [RobotId]);
            _ ->
              lager:warning("[Robot-~p] Received unknown ErrorMsg: ~p~n", [RobotId, ErrorCode])
          end;
        undefined ->
          lager:warning("[Robot-~p] Discard unknown reply: ~p~n", [RobotId, ReplyMsg])
      end,
      loop(RobotFSMId, RobotId, MessageDealer, RobotTimer);
    stop ->
      lager:warning("[Robot-~p] Callback received stop message.", [RobotId]),
      RobotTimer ! stop,
      gen_fsm:send_all_state_event(RobotFSMId, stop)
  after 10000 ->
    lager:warning("[Robot-~p] Receiving timeout", [RobotId]),
    gen_fsm:send_all_state_event(RobotFSMId, stop)
  end.