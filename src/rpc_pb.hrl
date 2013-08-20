-ifndef(TRANSUNIT_PB_H).
-define(TRANSUNIT_PB_H, true).
-record(transunit, {
    sn = erlang:error({required, sn}),
    '$extensions' = dict:new()
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

