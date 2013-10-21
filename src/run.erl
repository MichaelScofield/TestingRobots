%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Oct 2013 5:20 PM
%%%-------------------------------------------------------------------
-module(run).
-author("lfc").

%% API
-export([start/0]).

start() ->
  robot_ss:start_link(1,10,5),
  receive after infinity -> stop end.