-module(game_controller).

-behaviour(gen_server).

-export([ start_link/1 ]).

-export([
	init/1, terminate/2,
	handle_info/2, handle_call/3, handle_cast/2,
	code_change/3
	]).

-export([
	update/2
	]).

-record(state, {
		game_id,
		profile_id
		}).

start_link(Game) ->
	gen_server:start_link(?MODULE, Game, []).

init(Game) -> {ok, Game}.

update(PID, Location) ->
	gen_server:cast(self(), {location, {PID, Location}}).

handle_cast({location, _Data}, Game) ->
	{noreply, Game};

handle_cast(stop, Game) -> {stop, normal, Game}.

handle_call(_R, _F, _S) -> {stop, bad_call, undefined}.
handle_info(_I, _S) -> {stop, bad_call, undefined}.

terminate(_R, _Game) -> ok.

code_change(_Old, S, _Extra) -> {ok, S}.
