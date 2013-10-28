%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Oct 2013 5:20 PM
%%%-------------------------------------------------------------------
-module(run_robots).
-author("lfc").

-behaviour(application).

%% API
-export([start/2, stop/1, start/0]).

start() ->
  application:start(?MODULE).

start(normal, StartArgs) ->
  robot_ss:start_link(StartArgs).

stop(_State) ->
  ok.