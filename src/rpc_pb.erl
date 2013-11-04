-file("src/rpc_pb.erl", 1).

-module(rpc_pb).

-export([encode_enterarenarequest/1,
	 decode_enterarenarequest/1,
	 delimited_decode_enterarenarequest/1,
	 encode_avatarmovemessage/1, decode_avatarmovemessage/1,
	 delimited_decode_avatarmovemessage/1,
	 encode_pingmessage/1, decode_pingmessage/1,
	 delimited_decode_pingmessage/1,
	 encode_avatarentervisionmessage/1,
	 decode_avatarentervisionmessage/1,
	 delimited_decode_avatarentervisionmessage/1,
	 encode_mapinitmessage/1, decode_mapinitmessage/1,
	 delimited_decode_mapinitmessage/1, encode_talentinfo/1,
	 decode_talentinfo/1, delimited_decode_talentinfo/1,
	 encode_basicitem/1, decode_basicitem/1,
	 delimited_decode_basicitem/1, encode_formula/1,
	 decode_formula/1, delimited_decode_formula/1,
	 encode_item/1, decode_item/1, delimited_decode_item/1,
	 encode_bagitem/1, decode_bagitem/1,
	 delimited_decode_bagitem/1, encode_accountinfo/1,
	 decode_accountinfo/1, delimited_decode_accountinfo/1,
	 encode_loginreply/1, decode_loginreply/1,
	 delimited_decode_loginreply/1, encode_loginrequest/1,
	 decode_loginrequest/1, delimited_decode_loginrequest/1,
	 encode_createavatarrequest/1,
	 decode_createavatarrequest/1,
	 delimited_decode_createavatarrequest/1,
	 encode_characterinfo/1, decode_characterinfo/1,
	 delimited_decode_characterinfo/1,
	 encode_occupationupgradeinfo/1,
	 decode_occupationupgradeinfo/1,
	 delimited_decode_occupationupgradeinfo/1,
	 encode_unitattribute/1, decode_unitattribute/1,
	 delimited_decode_unitattribute/1, encode_geminfo/1,
	 decode_geminfo/1, delimited_decode_geminfo/1,
	 encode_equipment/1, decode_equipment/1,
	 delimited_decode_equipment/1, encode_errormessage/1,
	 decode_errormessage/1, delimited_decode_errormessage/1,
	 encode_transunit/1, decode_transunit/1,
	 delimited_decode_transunit/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-record(enterarenarequest, {}).

-record(avatarmovemessage,
	{grid_x, grid_y, account_id}).

-record(pingmessage, {}).

-record(avatarentervisionmessage,
	{account_id, character_info, grid_x, grid_y}).

-record(mapinitmessage, {city_id, x, y, avatars}).

-record(talentinfo,
	{meta_id, account_id, talent_level}).

-record(basicitem, {id, meta_id, amount}).

-record(formula, {id, meta_id}).

-record(item, {type, equipment, formula, basic_item}).

-record(bagitem, {id, account_id, item, position}).

-record(accountinfo,
	{id, diamond, gold, power, power_seconds_to_update,
	 talent, reputation}).

-record(loginreply,
	{errorcode, characterinfo, accountinfo, bagitems,
	 talentinfo, map, unlockedcityid}).

-record(loginrequest,
	{device_id, client_version, meta_crc32}).

-record(createavatarrequest,
	{device_id, name, meta_id}).

-record(characterinfo,
	{id, accountid, name, metaid, exp, level, place,
	 position, hasequipments, equipments, unitattributes,
	 occupationupgradeinfo}).

-record(occupationupgradeinfo,
	{characterid, metaid, exp}).

-record(unitattribute,
	{health_point, health_point_max, strength, technic,
	 mental, physical_attack, physical_defence, skill_attack,
	 skill_defence, magic_attack, magic_defence,
	 critical_rating, critical_strength, toughness,
	 dodge_rating, hit_rating, parry_rating,
	 parry_break_rating, move_speed, rage, rage_max,
	 parry_parameter, recovery, m_brave, m_injury, p_brave,
	 p_injury, r_recovery, health_talent}).

-record(geminfo, {gem_id, gem_meta_id, upgraderate}).

-record(equipment, {id, metaid, strengthen, geminfo}).

-record(errormessage, {error}).

-record(transunit, {sn, '$extensions'}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_enterarenarequest(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_enterarenarequest(Record)
    when is_record(Record, enterarenarequest) ->
    encode(enterarenarequest, Record).

encode_avatarmovemessage(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_avatarmovemessage(Record)
    when is_record(Record, avatarmovemessage) ->
    encode(avatarmovemessage, Record).

encode_pingmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pingmessage(Record)
    when is_record(Record, pingmessage) ->
    encode(pingmessage, Record).

encode_avatarentervisionmessage(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_avatarentervisionmessage(Record)
    when is_record(Record, avatarentervisionmessage) ->
    encode(avatarentervisionmessage, Record).

encode_mapinitmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_mapinitmessage(Record)
    when is_record(Record, mapinitmessage) ->
    encode(mapinitmessage, Record).

encode_talentinfo(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_talentinfo(Record)
    when is_record(Record, talentinfo) ->
    encode(talentinfo, Record).

encode_basicitem(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_basicitem(Record)
    when is_record(Record, basicitem) ->
    encode(basicitem, Record).

encode_formula(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_formula(Record)
    when is_record(Record, formula) ->
    encode(formula, Record).

encode_item(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_item(Record) when is_record(Record, item) ->
    encode(item, Record).

encode_bagitem(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_bagitem(Record)
    when is_record(Record, bagitem) ->
    encode(bagitem, Record).

encode_accountinfo(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_accountinfo(Record)
    when is_record(Record, accountinfo) ->
    encode(accountinfo, Record).

encode_loginreply(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_loginreply(Record)
    when is_record(Record, loginreply) ->
    encode(loginreply, Record).

encode_loginrequest(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_loginrequest(Record)
    when is_record(Record, loginrequest) ->
    encode(loginrequest, Record).

encode_createavatarrequest(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_createavatarrequest(Record)
    when is_record(Record, createavatarrequest) ->
    encode(createavatarrequest, Record).

encode_characterinfo(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_characterinfo(Record)
    when is_record(Record, characterinfo) ->
    encode(characterinfo, Record).

encode_occupationupgradeinfo(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_occupationupgradeinfo(Record)
    when is_record(Record, occupationupgradeinfo) ->
    encode(occupationupgradeinfo, Record).

encode_unitattribute(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_unitattribute(Record)
    when is_record(Record, unitattribute) ->
    encode(unitattribute, Record).

encode_geminfo(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_geminfo(Record)
    when is_record(Record, geminfo) ->
    encode(geminfo, Record).

encode_equipment(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_equipment(Record)
    when is_record(Record, equipment) ->
    encode(equipment, Record).

encode_errormessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_errormessage(Record)
    when is_record(Record, errormessage) ->
    encode(errormessage, Record).

encode_transunit(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_transunit(Record)
    when is_record(Record, transunit) ->
    encode(transunit, Record).

encode(transunit, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(transunit, Record) ->
    [iolist(transunit, Record) | encode_extensions(Record)];
encode(errormessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(errormessage, Record) ->
    [iolist(errormessage, Record)
     | encode_extensions(Record)];
encode(equipment, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(equipment, Record) ->
    [iolist(equipment, Record) | encode_extensions(Record)];
encode(geminfo, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(geminfo, Record) ->
    [iolist(geminfo, Record) | encode_extensions(Record)];
encode(unitattribute, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(unitattribute, Record) ->
    [iolist(unitattribute, Record)
     | encode_extensions(Record)];
encode(occupationupgradeinfo, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(occupationupgradeinfo, Record) ->
    [iolist(occupationupgradeinfo, Record)
     | encode_extensions(Record)];
encode(characterinfo, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(characterinfo, Record) ->
    [iolist(characterinfo, Record)
     | encode_extensions(Record)];
encode(createavatarrequest, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(createavatarrequest, Record) ->
    [iolist(createavatarrequest, Record)
     | encode_extensions(Record)];
encode(loginrequest, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(loginrequest, Record) ->
    [iolist(loginrequest, Record)
     | encode_extensions(Record)];
encode(loginreply, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(loginreply, Record) ->
    [iolist(loginreply, Record)
     | encode_extensions(Record)];
encode(accountinfo, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(accountinfo, Record) ->
    [iolist(accountinfo, Record)
     | encode_extensions(Record)];
encode(bagitem, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(bagitem, Record) ->
    [iolist(bagitem, Record) | encode_extensions(Record)];
encode(item, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(item, Record) ->
    [iolist(item, Record) | encode_extensions(Record)];
encode(formula, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(formula, Record) ->
    [iolist(formula, Record) | encode_extensions(Record)];
encode(basicitem, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(basicitem, Record) ->
    [iolist(basicitem, Record) | encode_extensions(Record)];
encode(talentinfo, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(talentinfo, Record) ->
    [iolist(talentinfo, Record)
     | encode_extensions(Record)];
encode(mapinitmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(mapinitmessage, Record) ->
    [iolist(mapinitmessage, Record)
     | encode_extensions(Record)];
encode(avatarentervisionmessage, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(avatarentervisionmessage, Record) ->
    [iolist(avatarentervisionmessage, Record)
     | encode_extensions(Record)];
encode(pingmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pingmessage, Record) ->
    [iolist(pingmessage, Record)
     | encode_extensions(Record)];
encode(avatarmovemessage, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(avatarmovemessage, Record) ->
    [iolist(avatarmovemessage, Record)
     | encode_extensions(Record)];
encode(enterarenarequest, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(enterarenarequest, Record) ->
    [iolist(enterarenarequest, Record)
     | encode_extensions(Record)].

encode_extensions(#transunit{'$extensions' =
				 Extends}) ->
    [pack(Key, Optionalness, Data, Type, Accer)
     || {Key, {Optionalness, Data, Type, Accer}}
	    <- dict:to_list(Extends)];
encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(transunit, Record) ->
    [pack(1, required,
	  with_default(Record#transunit.sn, none), int32, [])];
iolist(errormessage, Record) ->
    [pack(1, required,
	  with_default(Record#errormessage.error, none),
	  errorcode, [])];
iolist(equipment, Record) ->
    [pack(1, required,
	  with_default(Record#equipment.id, none), int64, []),
     pack(2, required,
	  with_default(Record#equipment.metaid, none), int32, []),
     pack(3, required,
	  with_default(Record#equipment.strengthen, none), int32,
	  []),
     pack(4, repeated,
	  with_default(Record#equipment.geminfo, none), geminfo,
	  [])];
iolist(geminfo, Record) ->
    [pack(1, required,
	  with_default(Record#geminfo.gem_id, none), int64, []),
     pack(2, required,
	  with_default(Record#geminfo.gem_meta_id, none), int32,
	  []),
     pack(3, required,
	  with_default(Record#geminfo.upgraderate, none), int32,
	  [])];
iolist(unitattribute, Record) ->
    [pack(1, optional,
	  with_default(Record#unitattribute.health_point, none),
	  int32, []),
     pack(2, optional,
	  with_default(Record#unitattribute.health_point_max,
		       none),
	  int32, []),
     pack(3, optional,
	  with_default(Record#unitattribute.strength, none),
	  int32, []),
     pack(4, optional,
	  with_default(Record#unitattribute.technic, none), int32,
	  []),
     pack(5, optional,
	  with_default(Record#unitattribute.mental, none), int32,
	  []),
     pack(6, optional,
	  with_default(Record#unitattribute.physical_attack,
		       none),
	  int32, []),
     pack(7, optional,
	  with_default(Record#unitattribute.physical_defence,
		       none),
	  int32, []),
     pack(8, optional,
	  with_default(Record#unitattribute.skill_attack, none),
	  int32, []),
     pack(9, optional,
	  with_default(Record#unitattribute.skill_defence, none),
	  int32, []),
     pack(10, optional,
	  with_default(Record#unitattribute.magic_attack, none),
	  int32, []),
     pack(11, optional,
	  with_default(Record#unitattribute.magic_defence, none),
	  int32, []),
     pack(12, optional,
	  with_default(Record#unitattribute.critical_rating,
		       none),
	  int32, []),
     pack(13, optional,
	  with_default(Record#unitattribute.critical_strength,
		       none),
	  int32, []),
     pack(14, optional,
	  with_default(Record#unitattribute.toughness, none),
	  int32, []),
     pack(15, optional,
	  with_default(Record#unitattribute.dodge_rating, none),
	  int32, []),
     pack(16, optional,
	  with_default(Record#unitattribute.hit_rating, none),
	  int32, []),
     pack(17, optional,
	  with_default(Record#unitattribute.parry_rating, none),
	  int32, []),
     pack(18, optional,
	  with_default(Record#unitattribute.parry_break_rating,
		       none),
	  int32, []),
     pack(19, optional,
	  with_default(Record#unitattribute.move_speed, none),
	  int32, []),
     pack(20, optional,
	  with_default(Record#unitattribute.rage, none), int32,
	  []),
     pack(21, optional,
	  with_default(Record#unitattribute.rage_max, none),
	  int32, []),
     pack(22, optional,
	  with_default(Record#unitattribute.parry_parameter,
		       none),
	  int32, []),
     pack(23, optional,
	  with_default(Record#unitattribute.recovery, none),
	  int32, []),
     pack(24, optional,
	  with_default(Record#unitattribute.m_brave, none), int32,
	  []),
     pack(25, optional,
	  with_default(Record#unitattribute.m_injury, none),
	  int32, []),
     pack(27, optional,
	  with_default(Record#unitattribute.p_brave, none), int32,
	  []),
     pack(28, optional,
	  with_default(Record#unitattribute.p_injury, none),
	  int32, []),
     pack(29, optional,
	  with_default(Record#unitattribute.r_recovery, none),
	  int32, []),
     pack(30, optional,
	  with_default(Record#unitattribute.health_talent, none),
	  int32, [])];
iolist(occupationupgradeinfo, Record) ->
    [pack(1, required,
	  with_default(Record#occupationupgradeinfo.characterid,
		       none),
	  int64, []),
     pack(2, required,
	  with_default(Record#occupationupgradeinfo.metaid, none),
	  int32, []),
     pack(3, required,
	  with_default(Record#occupationupgradeinfo.exp, none),
	  int64, [])];
iolist(characterinfo, Record) ->
    [pack(1, required,
	  with_default(Record#characterinfo.id, none), int64, []),
     pack(2, required,
	  with_default(Record#characterinfo.accountid, none),
	  int64, []),
     pack(3, optional,
	  with_default(Record#characterinfo.name, none), string,
	  []),
     pack(4, optional,
	  with_default(Record#characterinfo.metaid, none), int32,
	  []),
     pack(8, optional,
	  with_default(Record#characterinfo.exp, none), int64,
	  []),
     pack(9, optional,
	  with_default(Record#characterinfo.level, none), int32,
	  []),
     pack(11, optional,
	  with_default(Record#characterinfo.place, none),
	  characterplace, []),
     pack(12, optional,
	  with_default(Record#characterinfo.position, none),
	  int32, []),
     pack(13, required,
	  with_default(Record#characterinfo.hasequipments, none),
	  bool, []),
     pack(14, repeated,
	  with_default(Record#characterinfo.equipments, none),
	  equipment, []),
     pack(15, optional,
	  with_default(Record#characterinfo.unitattributes, none),
	  unitattribute, []),
     pack(16, optional,
	  with_default(Record#characterinfo.occupationupgradeinfo,
		       none),
	  occupationupgradeinfo, [])];
iolist(createavatarrequest, Record) ->
    [pack(1, required,
	  with_default(Record#createavatarrequest.device_id,
		       none),
	  string, []),
     pack(2, required,
	  with_default(Record#createavatarrequest.name, none),
	  string, []),
     pack(3, required,
	  with_default(Record#createavatarrequest.meta_id, none),
	  int32, [])];
iolist(loginrequest, Record) ->
    [pack(1, required,
	  with_default(Record#loginrequest.device_id, none),
	  string, []),
     pack(2, required,
	  with_default(Record#loginrequest.client_version, none),
	  string, []),
     pack(3, required,
	  with_default(Record#loginrequest.meta_crc32, none),
	  string, [])];
iolist(loginreply, Record) ->
    [pack(1, required,
	  with_default(Record#loginreply.errorcode, none),
	  errorcode, []),
     pack(2, repeated,
	  with_default(Record#loginreply.characterinfo, none),
	  characterinfo, []),
     pack(3, required,
	  with_default(Record#loginreply.accountinfo, none),
	  accountinfo, []),
     pack(5, repeated,
	  with_default(Record#loginreply.bagitems, none), bagitem,
	  []),
     pack(6, repeated,
	  with_default(Record#loginreply.talentinfo, none),
	  talentinfo, []),
     pack(7, required,
	  with_default(Record#loginreply.map, none),
	  mapinitmessage, []),
     pack(11, required,
	  with_default(Record#loginreply.unlockedcityid, none),
	  int32, [])];
iolist(accountinfo, Record) ->
    [pack(1, required,
	  with_default(Record#accountinfo.id, none), int64, []),
     pack(5, required,
	  with_default(Record#accountinfo.diamond, none), int32,
	  []),
     pack(6, required,
	  with_default(Record#accountinfo.gold, none), int32, []),
     pack(7, required,
	  with_default(Record#accountinfo.power, none), int32,
	  []),
     pack(8, required,
	  with_default(Record#accountinfo.power_seconds_to_update,
		       none),
	  int32, []),
     pack(9, required,
	  with_default(Record#accountinfo.talent, none), int32,
	  []),
     pack(10, required,
	  with_default(Record#accountinfo.reputation, none),
	  int32, [])];
iolist(bagitem, Record) ->
    [pack(1, required,
	  with_default(Record#bagitem.id, none), int64, []),
     pack(2, required,
	  with_default(Record#bagitem.account_id, none), int64,
	  []),
     pack(3, required,
	  with_default(Record#bagitem.item, none), item, []),
     pack(4, required,
	  with_default(Record#bagitem.position, none), int32,
	  [])];
iolist(item, Record) ->
    [pack(1, required, with_default(Record#item.type, none),
	  itemtype, []),
     pack(5, optional,
	  with_default(Record#item.equipment, none), equipment,
	  []),
     pack(6, optional,
	  with_default(Record#item.formula, none), formula, []),
     pack(7, optional,
	  with_default(Record#item.basic_item, none), basicitem,
	  [])];
iolist(formula, Record) ->
    [pack(1, required,
	  with_default(Record#formula.id, none), int64, []),
     pack(2, required,
	  with_default(Record#formula.meta_id, none), int32, [])];
iolist(basicitem, Record) ->
    [pack(1, required,
	  with_default(Record#basicitem.id, none), int64, []),
     pack(2, required,
	  with_default(Record#basicitem.meta_id, none), int32,
	  []),
     pack(3, required,
	  with_default(Record#basicitem.amount, none), int32,
	  [])];
iolist(talentinfo, Record) ->
    [pack(1, required,
	  with_default(Record#talentinfo.meta_id, none), int32,
	  []),
     pack(2, required,
	  with_default(Record#talentinfo.account_id, none), int64,
	  []),
     pack(3, required,
	  with_default(Record#talentinfo.talent_level, none),
	  int32, [])];
iolist(mapinitmessage, Record) ->
    [pack(1, required,
	  with_default(Record#mapinitmessage.city_id, none),
	  int32, []),
     pack(2, required,
	  with_default(Record#mapinitmessage.x, none), int32, []),
     pack(3, required,
	  with_default(Record#mapinitmessage.y, none), int32, []),
     pack(4, repeated,
	  with_default(Record#mapinitmessage.avatars, none),
	  avatarentervisionmessage, [])];
iolist(avatarentervisionmessage, Record) ->
    [pack(1, required,
	  with_default(Record#avatarentervisionmessage.account_id,
		       none),
	  int64, []),
     pack(2, required,
	  with_default(Record#avatarentervisionmessage.character_info,
		       none),
	  characterinfo, []),
     pack(3, required,
	  with_default(Record#avatarentervisionmessage.grid_x,
		       none),
	  int32, []),
     pack(4, required,
	  with_default(Record#avatarentervisionmessage.grid_y,
		       none),
	  int32, [])];
iolist(pingmessage, _Record) -> [];
iolist(avatarmovemessage, Record) ->
    [pack(1, required,
	  with_default(Record#avatarmovemessage.grid_x, none),
	  int32, []),
     pack(2, required,
	  with_default(Record#avatarmovemessage.grid_y, none),
	  int32, []),
     pack(3, required,
	  with_default(Record#avatarmovemessage.account_id, none),
	  int64, [])];
iolist(enterarenarequest, _Record) -> [].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(itemtype, 'FORMULA') -> 3;
enum_to_int(itemtype, 'EQUIPMENT') -> 2;
enum_to_int(itemtype, 'BASIC_ITEM') -> 1;
enum_to_int(itemtype, 'EMPTY') -> 0;
enum_to_int(characterplace, 'BAR') -> 2;
enum_to_int(characterplace, 'BATTLE_TEAM') -> 1;
enum_to_int(characterplace, 'BACKUP_TEAM') -> 0;
enum_to_int(errorcode, 'SYSTEM_ERROR') -> 999;
enum_to_int(errorcode, 'META_VERSION_MISMATCH') -> 82;
enum_to_int(errorcode, 'CLIENT_VERSION_MISMATCH') -> 81;
enum_to_int(errorcode, 'INVALID_PARAMETER') -> 8;
enum_to_int(errorcode, 'NO_SUCH_ACCOUNT') -> 7;
enum_to_int(errorcode, 'CLIENT_DISCONNECT') -> 2;
enum_to_int(errorcode, 'DUPLICATE_NAME') -> 1;
enum_to_int(errorcode, 'SUCCESS') -> 0.

int_to_enum(itemtype, 3) -> 'FORMULA';
int_to_enum(itemtype, 2) -> 'EQUIPMENT';
int_to_enum(itemtype, 1) -> 'BASIC_ITEM';
int_to_enum(itemtype, 0) -> 'EMPTY';
int_to_enum(characterplace, 2) -> 'BAR';
int_to_enum(characterplace, 1) -> 'BATTLE_TEAM';
int_to_enum(characterplace, 0) -> 'BACKUP_TEAM';
int_to_enum(errorcode, 999) -> 'SYSTEM_ERROR';
int_to_enum(errorcode, 82) -> 'META_VERSION_MISMATCH';
int_to_enum(errorcode, 81) -> 'CLIENT_VERSION_MISMATCH';
int_to_enum(errorcode, 8) -> 'INVALID_PARAMETER';
int_to_enum(errorcode, 7) -> 'NO_SUCH_ACCOUNT';
int_to_enum(errorcode, 2) -> 'CLIENT_DISCONNECT';
int_to_enum(errorcode, 1) -> 'DUPLICATE_NAME';
int_to_enum(errorcode, 0) -> 'SUCCESS';
int_to_enum(_, Val) -> Val.

decode_enterarenarequest(Bytes) when is_binary(Bytes) ->
    decode(enterarenarequest, Bytes).

decode_avatarmovemessage(Bytes) when is_binary(Bytes) ->
    decode(avatarmovemessage, Bytes).

decode_pingmessage(Bytes) when is_binary(Bytes) ->
    decode(pingmessage, Bytes).

decode_avatarentervisionmessage(Bytes)
    when is_binary(Bytes) ->
    decode(avatarentervisionmessage, Bytes).

decode_mapinitmessage(Bytes) when is_binary(Bytes) ->
    decode(mapinitmessage, Bytes).

decode_talentinfo(Bytes) when is_binary(Bytes) ->
    decode(talentinfo, Bytes).

decode_basicitem(Bytes) when is_binary(Bytes) ->
    decode(basicitem, Bytes).

decode_formula(Bytes) when is_binary(Bytes) ->
    decode(formula, Bytes).

decode_item(Bytes) when is_binary(Bytes) ->
    decode(item, Bytes).

decode_bagitem(Bytes) when is_binary(Bytes) ->
    decode(bagitem, Bytes).

decode_accountinfo(Bytes) when is_binary(Bytes) ->
    decode(accountinfo, Bytes).

decode_loginreply(Bytes) when is_binary(Bytes) ->
    decode(loginreply, Bytes).

decode_loginrequest(Bytes) when is_binary(Bytes) ->
    decode(loginrequest, Bytes).

decode_createavatarrequest(Bytes)
    when is_binary(Bytes) ->
    decode(createavatarrequest, Bytes).

decode_characterinfo(Bytes) when is_binary(Bytes) ->
    decode(characterinfo, Bytes).

decode_occupationupgradeinfo(Bytes)
    when is_binary(Bytes) ->
    decode(occupationupgradeinfo, Bytes).

decode_unitattribute(Bytes) when is_binary(Bytes) ->
    decode(unitattribute, Bytes).

decode_geminfo(Bytes) when is_binary(Bytes) ->
    decode(geminfo, Bytes).

decode_equipment(Bytes) when is_binary(Bytes) ->
    decode(equipment, Bytes).

decode_errormessage(Bytes) when is_binary(Bytes) ->
    decode(errormessage, Bytes).

decode_transunit(Bytes) when is_binary(Bytes) ->
    decode(transunit, Bytes).

delimited_decode_transunit(Bytes) ->
    delimited_decode(transunit, Bytes).

delimited_decode_errormessage(Bytes) ->
    delimited_decode(errormessage, Bytes).

delimited_decode_equipment(Bytes) ->
    delimited_decode(equipment, Bytes).

delimited_decode_geminfo(Bytes) ->
    delimited_decode(geminfo, Bytes).

delimited_decode_unitattribute(Bytes) ->
    delimited_decode(unitattribute, Bytes).

delimited_decode_occupationupgradeinfo(Bytes) ->
    delimited_decode(occupationupgradeinfo, Bytes).

delimited_decode_characterinfo(Bytes) ->
    delimited_decode(characterinfo, Bytes).

delimited_decode_createavatarrequest(Bytes) ->
    delimited_decode(createavatarrequest, Bytes).

delimited_decode_loginrequest(Bytes) ->
    delimited_decode(loginrequest, Bytes).

delimited_decode_loginreply(Bytes) ->
    delimited_decode(loginreply, Bytes).

delimited_decode_accountinfo(Bytes) ->
    delimited_decode(accountinfo, Bytes).

delimited_decode_bagitem(Bytes) ->
    delimited_decode(bagitem, Bytes).

delimited_decode_item(Bytes) ->
    delimited_decode(item, Bytes).

delimited_decode_formula(Bytes) ->
    delimited_decode(formula, Bytes).

delimited_decode_basicitem(Bytes) ->
    delimited_decode(basicitem, Bytes).

delimited_decode_talentinfo(Bytes) ->
    delimited_decode(talentinfo, Bytes).

delimited_decode_mapinitmessage(Bytes) ->
    delimited_decode(mapinitmessage, Bytes).

delimited_decode_avatarentervisionmessage(Bytes) ->
    delimited_decode(avatarentervisionmessage, Bytes).

delimited_decode_pingmessage(Bytes) ->
    delimited_decode(pingmessage, Bytes).

delimited_decode_avatarmovemessage(Bytes) ->
    delimited_decode(avatarmovemessage, Bytes).

delimited_decode_enterarenarequest(Bytes) ->
    delimited_decode(enterarenarequest, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(transunit, Bytes) when is_binary(Bytes) ->
    Types = [{1, sn, int32, []}],
    Defaults = [{false, '$extensions',
		 {dict, 0, 16, 16, 8, 80, 48,
		  {[], [], [], [], [], [], [], [], [], [], [], [], [], [],
		   [], []},
		  {{[], [], [], [], [], [], [], [], [], [], [], [], [],
		    [], [], []}}}}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(transunit, Decoded);
decode(errormessage, Bytes) when is_binary(Bytes) ->
    Types = [{1, error, errorcode, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(errormessage, Decoded);
decode(equipment, Bytes) when is_binary(Bytes) ->
    Types = [{4, geminfo, geminfo, [is_record, repeated]},
	     {3, strengthen, int32, []}, {2, metaid, int32, []},
	     {1, id, int64, []}],
    Defaults = [{4, geminfo, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(equipment, Decoded);
decode(geminfo, Bytes) when is_binary(Bytes) ->
    Types = [{3, upgraderate, int32, []},
	     {2, gem_meta_id, int32, []}, {1, gem_id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(geminfo, Decoded);
decode(unitattribute, Bytes) when is_binary(Bytes) ->
    Types = [{30, health_talent, int32, []},
	     {29, r_recovery, int32, []}, {28, p_injury, int32, []},
	     {27, p_brave, int32, []}, {25, m_injury, int32, []},
	     {24, m_brave, int32, []}, {23, recovery, int32, []},
	     {22, parry_parameter, int32, []},
	     {21, rage_max, int32, []}, {20, rage, int32, []},
	     {19, move_speed, int32, []},
	     {18, parry_break_rating, int32, []},
	     {17, parry_rating, int32, []},
	     {16, hit_rating, int32, []},
	     {15, dodge_rating, int32, []},
	     {14, toughness, int32, []},
	     {13, critical_strength, int32, []},
	     {12, critical_rating, int32, []},
	     {11, magic_defence, int32, []},
	     {10, magic_attack, int32, []},
	     {9, skill_defence, int32, []},
	     {8, skill_attack, int32, []},
	     {7, physical_defence, int32, []},
	     {6, physical_attack, int32, []}, {5, mental, int32, []},
	     {4, technic, int32, []}, {3, strength, int32, []},
	     {2, health_point_max, int32, []},
	     {1, health_point, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(unitattribute, Decoded);
decode(occupationupgradeinfo, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, exp, int64, []}, {2, metaid, int32, []},
	     {1, characterid, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(occupationupgradeinfo, Decoded);
decode(characterinfo, Bytes) when is_binary(Bytes) ->
    Types = [{16, occupationupgradeinfo,
	      occupationupgradeinfo, [is_record]},
	     {15, unitattributes, unitattribute, [is_record]},
	     {14, equipments, equipment, [is_record, repeated]},
	     {13, hasequipments, bool, []},
	     {12, position, int32, []},
	     {11, place, characterplace, []}, {9, level, int32, []},
	     {8, exp, int64, []}, {4, metaid, int32, []},
	     {3, name, string, []}, {2, accountid, int64, []},
	     {1, id, int64, []}],
    Defaults = [{14, equipments, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(characterinfo, Decoded);
decode(createavatarrequest, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, meta_id, int32, []}, {2, name, string, []},
	     {1, device_id, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(createavatarrequest, Decoded);
decode(loginrequest, Bytes) when is_binary(Bytes) ->
    Types = [{3, meta_crc32, string, []},
	     {2, client_version, string, []},
	     {1, device_id, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(loginrequest, Decoded);
decode(loginreply, Bytes) when is_binary(Bytes) ->
    Types = [{11, unlockedcityid, int32, []},
	     {7, map, mapinitmessage, [is_record]},
	     {6, talentinfo, talentinfo, [is_record, repeated]},
	     {5, bagitems, bagitem, [is_record, repeated]},
	     {3, accountinfo, accountinfo, [is_record]},
	     {2, characterinfo, characterinfo,
	      [is_record, repeated]},
	     {1, errorcode, errorcode, []}],
    Defaults = [{2, characterinfo, []}, {5, bagitems, []},
		{6, talentinfo, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(loginreply, Decoded);
decode(accountinfo, Bytes) when is_binary(Bytes) ->
    Types = [{10, reputation, int32, []},
	     {9, talent, int32, []},
	     {8, power_seconds_to_update, int32, []},
	     {7, power, int32, []}, {6, gold, int32, []},
	     {5, diamond, int32, []}, {1, id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(accountinfo, Decoded);
decode(bagitem, Bytes) when is_binary(Bytes) ->
    Types = [{4, position, int32, []},
	     {3, item, item, [is_record]},
	     {2, account_id, int64, []}, {1, id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(bagitem, Decoded);
decode(item, Bytes) when is_binary(Bytes) ->
    Types = [{7, basic_item, basicitem, [is_record]},
	     {6, formula, formula, [is_record]},
	     {5, equipment, equipment, [is_record]},
	     {1, type, itemtype, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(item, Decoded);
decode(formula, Bytes) when is_binary(Bytes) ->
    Types = [{2, meta_id, int32, []}, {1, id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(formula, Decoded);
decode(basicitem, Bytes) when is_binary(Bytes) ->
    Types = [{3, amount, int32, []},
	     {2, meta_id, int32, []}, {1, id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(basicitem, Decoded);
decode(talentinfo, Bytes) when is_binary(Bytes) ->
    Types = [{3, talent_level, int32, []},
	     {2, account_id, int64, []}, {1, meta_id, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(talentinfo, Decoded);
decode(mapinitmessage, Bytes) when is_binary(Bytes) ->
    Types = [{4, avatars, avatarentervisionmessage,
	      [is_record, repeated]},
	     {3, y, int32, []}, {2, x, int32, []},
	     {1, city_id, int32, []}],
    Defaults = [{4, avatars, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(mapinitmessage, Decoded);
decode(avatarentervisionmessage, Bytes)
    when is_binary(Bytes) ->
    Types = [{4, grid_y, int32, []}, {3, grid_x, int32, []},
	     {2, character_info, characterinfo, [is_record]},
	     {1, account_id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(avatarentervisionmessage, Decoded);
decode(pingmessage, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pingmessage, Decoded);
decode(avatarmovemessage, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, account_id, int64, []},
	     {2, grid_y, int32, []}, {1, grid_x, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(avatarmovemessage, Decoded);
decode(enterarenarequest, Bytes)
    when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(enterarenarequest, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(transunit, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       transunit),
						   Record, Name, Val)
			  end,
			  #transunit{}, DecodedTuples),
    decode_extensions(Record1);
to_record(errormessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       errormessage),
						   Record, Name, Val)
			  end,
			  #errormessage{}, DecodedTuples),
    Record1;
to_record(equipment, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       equipment),
						   Record, Name, Val)
			  end,
			  #equipment{}, DecodedTuples),
    Record1;
to_record(geminfo, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, geminfo),
						   Record, Name, Val)
			  end,
			  #geminfo{}, DecodedTuples),
    Record1;
to_record(unitattribute, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       unitattribute),
						   Record, Name, Val)
			  end,
			  #unitattribute{}, DecodedTuples),
    Record1;
to_record(occupationupgradeinfo, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       occupationupgradeinfo),
						   Record, Name, Val)
			  end,
			  #occupationupgradeinfo{}, DecodedTuples),
    Record1;
to_record(characterinfo, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       characterinfo),
						   Record, Name, Val)
			  end,
			  #characterinfo{}, DecodedTuples),
    Record1;
to_record(createavatarrequest, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       createavatarrequest),
						   Record, Name, Val)
			  end,
			  #createavatarrequest{}, DecodedTuples),
    Record1;
to_record(loginrequest, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       loginrequest),
						   Record, Name, Val)
			  end,
			  #loginrequest{}, DecodedTuples),
    Record1;
to_record(loginreply, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       loginreply),
						   Record, Name, Val)
			  end,
			  #loginreply{}, DecodedTuples),
    Record1;
to_record(accountinfo, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       accountinfo),
						   Record, Name, Val)
			  end,
			  #accountinfo{}, DecodedTuples),
    Record1;
to_record(bagitem, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, bagitem),
						   Record, Name, Val)
			  end,
			  #bagitem{}, DecodedTuples),
    Record1;
to_record(item, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, item),
						   Record, Name, Val)
			  end,
			  #item{}, DecodedTuples),
    Record1;
to_record(formula, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, formula),
						   Record, Name, Val)
			  end,
			  #formula{}, DecodedTuples),
    Record1;
to_record(basicitem, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       basicitem),
						   Record, Name, Val)
			  end,
			  #basicitem{}, DecodedTuples),
    Record1;
to_record(talentinfo, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       talentinfo),
						   Record, Name, Val)
			  end,
			  #talentinfo{}, DecodedTuples),
    Record1;
to_record(mapinitmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       mapinitmessage),
						   Record, Name, Val)
			  end,
			  #mapinitmessage{}, DecodedTuples),
    Record1;
to_record(avatarentervisionmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       avatarentervisionmessage),
						   Record, Name, Val)
			  end,
			  #avatarentervisionmessage{}, DecodedTuples),
    Record1;
to_record(pingmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pingmessage),
						   Record, Name, Val)
			  end,
			  #pingmessage{}, DecodedTuples),
    Record1;
to_record(avatarmovemessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       avatarmovemessage),
						   Record, Name, Val)
			  end,
			  #avatarmovemessage{}, DecodedTuples),
    Record1;
to_record(enterarenarequest, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       enterarenarequest),
						   Record, Name, Val)
			  end,
			  #enterarenarequest{}, DecodedTuples),
    Record1.

decode_extensions(#transunit{'$extensions' =
				 Extensions} =
		      Record) ->
    Types = [{9999, errormessage, errormessage,
	      [is_record]},
	     {9998, pingmessage, pingmessage, [is_record]},
	     {1301, enterarenarequest, enterarenarequest,
	      [is_record]},
	     {1015, mapInitMessage, mapinitmessage, [is_record]},
	     {1005, avatarEnterVisionMessage,
	      avatarentervisionmessage, [is_record]},
	     {1004, avatarmovemessage, avatarmovemessage,
	      [is_record]},
	     {1003, createAvatarRequest, createavatarrequest,
	      [is_record]},
	     {1002, loginReply, loginreply, [is_record]},
	     {1001, loginRequest, loginrequest, [is_record]},
	     {1000, accountInfo, accountinfo, [is_record]},
	     {1, sn, int32, []}, {1, sn, int32, []},
	     {1, sn, int32, []}, {1, sn, int32, []},
	     {1, sn, int32, []}, {1, sn, int32, []},
	     {1, sn, int32, []}, {1, sn, int32, []},
	     {1, sn, int32, []}, {1, sn, int32, []}],
    NewExtensions = decode_extensions(Types,
				      dict:to_list(Extensions), []),
    Record#transunit{'$extensions' = NewExtensions};
decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(#transunit{'$extensions' =
			      Extensions}) ->
    dict:size(Extensions);
extension_size(_) -> 0.

has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      9999) ->
    dict:is_key(9999, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      errormessage) ->
    dict:is_key(errormessage, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1003) ->
    dict:is_key(1003, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      createavatarrequest) ->
    dict:is_key(createavatarrequest, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1001) ->
    dict:is_key(1001, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      loginrequest) ->
    dict:is_key(loginrequest, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1002) ->
    dict:is_key(1002, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      loginreply) ->
    dict:is_key(loginreply, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1000) ->
    dict:is_key(1000, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      accountinfo) ->
    dict:is_key(accountinfo, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1015) ->
    dict:is_key(1015, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      mapinitmessage) ->
    dict:is_key(mapinitmessage, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1005) ->
    dict:is_key(1005, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      avatarentervisionmessage) ->
    dict:is_key(avatarentervisionmessage, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      9998) ->
    dict:is_key(9998, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      pingmessage) ->
    dict:is_key(pingmessage, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1004) ->
    dict:is_key(1004, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      avatarmovemessage) ->
    dict:is_key(avatarmovemessage, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1) ->
    dict:is_key(1, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      sn) ->
    dict:is_key(sn, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      1301) ->
    dict:is_key(1301, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      enterarenarequest) ->
    dict:is_key(enterarenarequest, Extensions);
has_extension(_Record, _FieldName) -> false.

get_extension(Record, enterarenarequest)
    when is_record(Record, transunit) ->
    get_extension(Record, 1301);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, avatarmovemessage)
    when is_record(Record, transunit) ->
    get_extension(Record, 1004);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, pingmessage)
    when is_record(Record, transunit) ->
    get_extension(Record, 9998);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, avatarentervisionmessage)
    when is_record(Record, transunit) ->
    get_extension(Record, 1005);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, mapinitmessage)
    when is_record(Record, transunit) ->
    get_extension(Record, 1015);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, accountinfo)
    when is_record(Record, transunit) ->
    get_extension(Record, 1000);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, loginreply)
    when is_record(Record, transunit) ->
    get_extension(Record, 1002);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, loginrequest)
    when is_record(Record, transunit) ->
    get_extension(Record, 1001);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, createavatarrequest)
    when is_record(Record, transunit) ->
    get_extension(Record, 1003);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(Record, errormessage)
    when is_record(Record, transunit) ->
    get_extension(Record, 9999);
get_extension(Record, sn)
    when is_record(Record, transunit) ->
    get_extension(Record, 1);
get_extension(#transunit{'$extensions' = Extensions},
	      Int)
    when is_integer(Int) ->
    case dict:find(Int, Extensions) of
      {ok, {_Rule, Value, _Type, _Opts}} -> {ok, Value};
      {ok, Binary} -> {raw, Binary};
      error -> undefined
    end;
get_extension(_Record, _FieldName) -> undefined.

set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      errormessage, Value) ->
    NewExtends = dict:store(9999,
			    {optional, Value, errormessage, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      createavatarrequest, Value) ->
    NewExtends = dict:store(1003,
			    {optional, Value, createavatarrequest, none},
			    Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      loginrequest, Value) ->
    NewExtends = dict:store(1001,
			    {optional, Value, loginrequest, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      loginreply, Value) ->
    NewExtends = dict:store(1002,
			    {optional, Value, loginreply, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      accountinfo, Value) ->
    NewExtends = dict:store(1000,
			    {optional, Value, accountinfo, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      mapinitmessage, Value) ->
    NewExtends = dict:store(1015,
			    {optional, Value, mapinitmessage, none},
			    Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      avatarentervisionmessage, Value) ->
    NewExtends = dict:store(1005,
			    {optional, Value, avatarentervisionmessage, none},
			    Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      pingmessage, Value) ->
    NewExtends = dict:store(9998,
			    {optional, Value, pingmessage, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      avatarmovemessage, Value) ->
    NewExtends = dict:store(1004,
			    {optional, Value, avatarmovemessage, none},
			    Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      sn, Value) ->
    NewExtends = dict:store(1,
			    {required, Value, int32, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(#transunit{'$extensions' = Extensions} =
		  Record,
	      enterarenarequest, Value) ->
    NewExtends = dict:store(1301,
			    {optional, Value, enterarenarequest, none},
			    Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(Record, _, _) -> {error, Record}.

