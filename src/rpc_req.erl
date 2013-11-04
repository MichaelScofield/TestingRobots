%% Copyright
-module(rpc_req).
-author("lfc").

-include("rpc_pb.hrl").

%% API
-export([create_account_req/1, login_req/1, ping/0, move/3, enter_arena_req/0, change_city_req/1]).

ping() ->
  Message = #pingmessage{},
  wrap_transunit(Message).

move(X, Y, AccountId) ->
  Message = #avatarmovemessage{grid_x = X, grid_y = Y, account_id = AccountId},
  wrap_transunit(Message).

create_account_req(RobotId) ->
  Id = integer_to_list(RobotId),
  DeviceId = "Erobot" ++ Id,
  Name = "Robot" ++ Id,
  CharacterIds = {131012, 131042, 131032},
  Random = gen_server:call(robot_status, {get, next_random, 3}),
  CharacterId = element(Random, CharacterIds),
  Message = #createavatarrequest{device_id = DeviceId, name = Name, meta_id = CharacterId},
  wrap_transunit(Message).

login_req(RobotId) ->
  Id = integer_to_list(RobotId),
  DeviceId = "robot-!@#-" ++ Id,
  Message = #loginrequest{device_id = DeviceId, client_version = "0.5.0", meta_crc32 = "8414FF96"},
  wrap_transunit(Message).

enter_arena_req() ->
  wrap_transunit(#enterarenarequest{}).

change_city_req(CityId) ->
  wrap_transunit(#changemaprequest{city_meta_id = CityId}).

wrap_transunit(Message) ->
  {_, Second, MicroSec} = now(),
  Sn = (Second - Second div 10000 * 10000) * 1000 + (MicroSec - MicroSec div 1000 * 1000),
  {ok, TransUnit} = rpc_pb:set_extension(#transunit{sn = Sn, '$extensions' = dict:new()}, element(1, Message), Message),
  TransUnit.