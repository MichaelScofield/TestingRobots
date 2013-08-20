-file("src/rpc_pb.erl", 1).

-module(rpc_pb).

-export([encode_loginrequest/1, decode_loginrequest/1,
	 delimited_decode_loginrequest/1, encode_transunit/1,
	 decode_transunit/1, delimited_decode_transunit/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-record(loginrequest,
	{device_id, client_version, meta_crc32}).

-record(transunit, {sn, '$extensions'}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_loginrequest(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_loginrequest(Record)
    when is_record(Record, loginrequest) ->
    encode(loginrequest, Record).

encode_transunit(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_transunit(Record)
    when is_record(Record, transunit) ->
    encode(transunit, Record).

encode(transunit, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(transunit, Record) ->
    [iolist(transunit, Record) | encode_extensions(Record)];
encode(loginrequest, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(loginrequest, Record) ->
    [iolist(loginrequest, Record)
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
iolist(loginrequest, Record) ->
    [pack(1, required,
	  with_default(Record#loginrequest.device_id, none),
	  string, []),
     pack(2, required,
	  with_default(Record#loginrequest.client_version, none),
	  string, []),
     pack(3, required,
	  with_default(Record#loginrequest.meta_crc32, none),
	  string, [])].

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

enum_to_int(pikachu, value) -> 1.

int_to_enum(_, Val) -> Val.

decode_loginrequest(Bytes) when is_binary(Bytes) ->
    decode(loginrequest, Bytes).

decode_transunit(Bytes) when is_binary(Bytes) ->
    decode(transunit, Bytes).

delimited_decode_transunit(Bytes) ->
    delimited_decode(transunit, Bytes).

delimited_decode_loginrequest(Bytes) ->
    delimited_decode(loginrequest, Bytes).

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
decode(loginrequest, Bytes) when is_binary(Bytes) ->
    Types = [{3, meta_crc32, string, []},
	     {2, client_version, string, []},
	     {1, device_id, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(loginrequest, Decoded).

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
to_record(loginrequest, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       loginrequest),
						   Record, Name, Val)
			  end,
			  #loginrequest{}, DecodedTuples),
    Record1.

decode_extensions(#transunit{'$extensions' =
				 Extensions} =
		      Record) ->
    Types = [{1001, msg, loginrequest, [is_record]},
	     {1, sn, int32, []}],
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
	      1001) ->
    dict:is_key(1001, Extensions);
has_extension(#transunit{'$extensions' = Extensions},
	      msg) ->
    dict:is_key(msg, Extensions);
has_extension(_Record, _FieldName) -> false.

get_extension(Record, msg)
    when is_record(Record, transunit) ->
    get_extension(Record, 1001);
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
	      msg, Value) ->
    NewExtends = dict:store(1001,
			    {optional, Value, loginrequest, none}, Extensions),
    {ok, Record#transunit{'$extensions' = NewExtends}};
set_extension(Record, _, _) -> {error, Record}.

