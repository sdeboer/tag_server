-module(game_controller).

-behaviour(gen_event).

-export([ start_link/1 ]).

-export([
	init/1, terminate/2,
	handle_info/2,
	handle_call/2,
	handle_event/2,
	code_change/3
	]).

-export([
	update/3,
	game_channel/1
	]).

-record(state, {
		game_id,
		profile_id,
		websocket,
		game_channel,
		publisher,
		sub_channel
		}).

start_link(Args) ->
	lager:debug("Got here ~p", [Args]),
	gen_server:start_link(?MODULE, Args, []).

init(Args) ->
	GID = proplists:get_value(game_id, Args),

	lager:debug("Starting ~p : ~p", [?MODULE, GID]),

	GidCh = game_channel(GID),

	ChSub = pubsub:new_sub(GidCh),

	{ok, Pub} = eredis:start_link(),

	gen_event:add_handler(ChSub, self()),

	{ok, #state{
			profile_id = proplists:get_value(profile_id, Args),
			game_id = GID,
			game_channel = GidCh,
			websocket = proplists:get_value(websocket, Args),
			publisher = Pub,
			sub_channel = ChSub
			}}.

update(GC, PID, Location) ->
	gen_event:call(GC, {location, PID, Location}).

handle_event(Event, S) ->
	lager:debug("GC Event ~p", [Event]),
	{ok, S}.

handle_call({location, PID, Location}, S) ->
	Msg = jiffy:encode([location, PID, Location]),
	eredis:q(S#state.publisher, ["PUBLISH", 
			S#state.game_channel, Msg]),
	{ok, S};

handle_call(_R, _S) -> {stop, bad_call, undefined}.

handle_info(stop, S) -> {stop, normal, S};

handle_info(_Info, S) -> {ok, S}.

terminate(_R, S) ->
	gen_event:delete_handler(S#state.sub_channel, self()),
	ok.

code_change(_Old, S, _Extra) -> {ok, S}.

game_channel(GID) ->
	erlang:binary_to_atom(iolist_to_binary([?MODULE_STRING, GID]), utf8).
