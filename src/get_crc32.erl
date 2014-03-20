%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2014 上午10:49
%%%-------------------------------------------------------------------
-module(get_crc32).
-author("lfc").

%% API
-export([get/0]).

get() ->
  inets:start(),
  {ok, {{_, 200, _}, _, Body}} = httpc:request("http://toruk.s1.afanda.com/meta/d.j"),
  Bytes = list_to_binary(Body),
  Crc32 = erlang:crc32(Bytes),
  integer_to_list(Crc32, 16).
