-ifndef(TRANSUNIT_PB_H).
-define(TRANSUNIT_PB_H, true).
-record(transunit, {
    sn = erlang:error({required, sn}),
    '$extensions' = dict:new()
}).
-endif.

-ifndef(ERRORMESSAGE_PB_H).
-define(ERRORMESSAGE_PB_H, true).
-record(errormessage, {
    error = erlang:error({required, error})
}).
-endif.

-ifndef(EQUIPMENT_PB_H).
-define(EQUIPMENT_PB_H, true).
-record(equipment, {
    id = erlang:error({required, id}),
    metaid = erlang:error({required, metaid}),
    strengthen = erlang:error({required, strengthen}),
    geminfo = []
}).
-endif.

-ifndef(GEMINFO_PB_H).
-define(GEMINFO_PB_H, true).
-record(geminfo, {
    gem_id = erlang:error({required, gem_id}),
    gem_meta_id = erlang:error({required, gem_meta_id}),
    upgraderate = erlang:error({required, upgraderate})
}).
-endif.

-ifndef(UNITATTRIBUTE_PB_H).
-define(UNITATTRIBUTE_PB_H, true).
-record(unitattribute, {
    health_point,
    health_point_max,
    strength,
    technic,
    mental,
    physical_attack,
    physical_defence,
    skill_attack,
    skill_defence,
    magic_attack,
    magic_defence,
    critical_rating,
    critical_strength,
    toughness,
    dodge_rating,
    hit_rating,
    parry_rating,
    parry_break_rating,
    move_speed,
    rage,
    rage_max,
    parry_parameter,
    recovery,
    m_brave,
    m_injury,
    p_brave,
    p_injury,
    r_recovery,
    health_talent
}).
-endif.

-ifndef(OCCUPATIONUPGRADEINFO_PB_H).
-define(OCCUPATIONUPGRADEINFO_PB_H, true).
-record(occupationupgradeinfo, {
    characterid = erlang:error({required, characterid}),
    metaid = erlang:error({required, metaid}),
    exp = erlang:error({required, exp})
}).
-endif.

-ifndef(CHARACTERINFO_PB_H).
-define(CHARACTERINFO_PB_H, true).
-record(characterinfo, {
    id = erlang:error({required, id}),
    accountid = erlang:error({required, accountid}),
    name,
    metaid,
    exp,
    level,
    place,
    position,
    hasequipments = erlang:error({required, hasequipments}),
    equipments = [],
    unitattributes,
    occupationupgradeinfo
}).
-endif.

-ifndef(CREATEAVATARREQUEST_PB_H).
-define(CREATEAVATARREQUEST_PB_H, true).
-record(createavatarrequest, {
    device_id = erlang:error({required, device_id}),
    name = erlang:error({required, name}),
    meta_id = erlang:error({required, meta_id})
}).
-endif.

-ifndef(LOGINREQUEST_PB_H).
-define(LOGINREQUEST_PB_H, true).
-record(loginrequest, {
    device_id = erlang:error({required, device_id}),
    client_version = erlang:error({required, client_version}),
    meta_crc32 = erlang:error({required, meta_crc32})
}).
-endif.

-ifndef(LOGINREPLY_PB_H).
-define(LOGINREPLY_PB_H, true).
-record(loginreply, {
    errorcode = erlang:error({required, errorcode}),
    characterinfo = [],
    accountinfo = erlang:error({required, accountinfo}),
    bagitems = [],
    talentinfo = [],
    map = erlang:error({required, map}),
    unlocked_city_id = []
}).
-endif.

-ifndef(ACCOUNTINFO_PB_H).
-define(ACCOUNTINFO_PB_H, true).
-record(accountinfo, {
    id = erlang:error({required, id}),
    diamond = erlang:error({required, diamond}),
    gold = erlang:error({required, gold}),
    power = erlang:error({required, power}),
    power_seconds_to_update = erlang:error({required, power_seconds_to_update}),
    talent = erlang:error({required, talent}),
    reputation = erlang:error({required, reputation})
}).
-endif.

-ifndef(BAGITEM_PB_H).
-define(BAGITEM_PB_H, true).
-record(bagitem, {
    id = erlang:error({required, id}),
    account_id = erlang:error({required, account_id}),
    item = erlang:error({required, item}),
    position = erlang:error({required, position})
}).
-endif.

-ifndef(ITEM_PB_H).
-define(ITEM_PB_H, true).
-record(item, {
    type = erlang:error({required, type}),
    equipment,
    formula,
    basic_item
}).
-endif.

-ifndef(FORMULA_PB_H).
-define(FORMULA_PB_H, true).
-record(formula, {
    id = erlang:error({required, id}),
    meta_id = erlang:error({required, meta_id})
}).
-endif.

-ifndef(BASICITEM_PB_H).
-define(BASICITEM_PB_H, true).
-record(basicitem, {
    id = erlang:error({required, id}),
    meta_id = erlang:error({required, meta_id}),
    amount = erlang:error({required, amount})
}).
-endif.

-ifndef(TALENTINFO_PB_H).
-define(TALENTINFO_PB_H, true).
-record(talentinfo, {
    meta_id = erlang:error({required, meta_id}),
    account_id = erlang:error({required, account_id}),
    talent_level = erlang:error({required, talent_level})
}).
-endif.

-ifndef(MAPINITMESSAGE_PB_H).
-define(MAPINITMESSAGE_PB_H, true).
-record(mapinitmessage, {
    city_id = erlang:error({required, city_id}),
    x = erlang:error({required, x}),
    y = erlang:error({required, y}),
    avatars = []
}).
-endif.

-ifndef(AVATARENTERVISIONMESSAGE_PB_H).
-define(AVATARENTERVISIONMESSAGE_PB_H, true).
-record(avatarentervisionmessage, {
    account_id = erlang:error({required, account_id}),
    character_info = erlang:error({required, character_info}),
    grid_x = erlang:error({required, grid_x}),
    grid_y = erlang:error({required, grid_y})
}).
-endif.

-ifndef(PINGMESSAGE_PB_H).
-define(PINGMESSAGE_PB_H, true).
-record(pingmessage, {
    
}).
-endif.

-ifndef(AVATARMOVEMESSAGE_PB_H).
-define(AVATARMOVEMESSAGE_PB_H, true).
-record(avatarmovemessage, {
    grid_x = erlang:error({required, grid_x}),
    grid_y = erlang:error({required, grid_y}),
    account_id = erlang:error({required, account_id})
}).
-endif.

-ifndef(ENTERARENAREQUEST_PB_H).
-define(ENTERARENAREQUEST_PB_H, true).
-record(enterarenarequest, {
    
}).
-endif.

-ifndef(CHANGEMAPREQUEST_PB_H).
-define(CHANGEMAPREQUEST_PB_H, true).
-record(changemaprequest, {
    city_meta_id = erlang:error({required, city_meta_id})
}).
-endif.

