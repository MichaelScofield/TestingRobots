%%%-------------------------------------------------------------------
%%% @author lfc
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2013 7:30 PM
%%%-------------------------------------------------------------------
-module(robot_status).
-author("lfc").

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

start_link() ->
  gen_server:start_link({local, robot_status}, ?MODULE, [], []).

init(_Args) ->
  random:seed(now()),
  State = dict:store(random_seed, random:seed(now()), dict:new()),
  {ok, State}.

handle_call({get, next_random, 0}, _From, State) ->
  {reply, 0, State};
handle_call({get, next_random, N}, _From, State) ->
  Seed = dict:fetch(random_seed, State),
  {Random, NextSeed} = random:uniform_s(N, Seed),
  NewState = dict:store(random_seed, NextSeed, State),
  {reply, Random, NewState};

handle_call({get, Key}, _From, State) ->
  {reply, dict:fetch(Key, State), State};
handle_call({set, Key, Value}, _From, State) ->
  NewState = dict:store(Key, Value, State),
  {reply, ok, NewState}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(_Reason, _State) ->
  erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
