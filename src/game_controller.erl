-module(game_controller).

-behaviour(gen_server).

-export([ start_link/1 ]).

-export([
	init/1, terminate/2,
	handle_info/2, handle_call/3, handle_cast/2,
	code_change/3
	]).

-export([
	update/3
	]).

-record(state, {
		game_id,
		profile_id,
		socket,
		pubsub
		}).

start_link(Args) ->
	lager:debug("Got here ~p", [Args]),
	gen_server:start_link(?MODULE, Args, []).

init(Args) ->
	lager:debug("init gc ~p", [proplists:get_value(game_id,Args)]),

	{ok, PS} = supervisor:start_child(pubsub_sup, [self()]),

	{ok, #state{
			game_id = proplists:get_value(game_id, Args),
			profile_id = proplists:get_value(profile_id, Args),
			socket = proplists:get_value(socket, Args),
			pubsub = PS
			}}.

update(GC, PID, Location) ->
	gen_server:cast(GC, {location, {PID, Location}}).

handle_cast({location, Data}, S) ->
	lager:debug("GC Recv ~p", [Data]),
	{noreply, S};

handle_cast(stop, S) -> {stop, normal, S}.

handle_call(_R, _F, _S) -> {stop, bad_call, undefined}.

% TODO this needs to handle the PUBSUB -> subscription callbacks
handle_info({message, _Ch, {geo, Geo}, _Sub}, S) ->
	ws_handler:geo(S#state.socket, Geo),
	pubsub:ack(S#state.pubsub).

terminate(_R, S) ->
	pubsub:stop(S#state.pubsub),
	ok.

code_change(_Old, S, _Extra) -> {ok, S}.
