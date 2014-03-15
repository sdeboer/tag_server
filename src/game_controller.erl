-module(game_controller).

-behaviour(gen_event).

-export([
	init/1, terminate/2,
	handle_info/2,
	handle_call/2,
	handle_event/2,
	code_change/3
	]).

-export([
	update/3,
	game_channels/1
	]).

-record(state, {
		game_id,
		profile_id,
		websocket,
		game_channel,
		publisher
		}).

init(Args) ->
	GID = proplists:get_value(game_id, Args),

	{_ErCh, ReCh} = game_channels(GID),

	{ok, Pub} = eredis:start_link(),

	{ok, #state{
			profile_id = proplists:get_value(profile_id, Args),
			game_id = GID,
			game_channel = ReCh,
			websocket = proplists:get_value(websocket, Args),
			publisher = Pub
			}}.

update(GC, PID, Location) ->
	gen_event:call(GC, {location, PID, Location}).

handle_event(Event, S) ->
	S#state.websocket ! {send, Event},
	{ok, S}.

handle_call({location, PID, Location}, S) ->
	Obj = [{playerID, PID}, {command, location} | Location],
	Msg = jiffy:encode({Obj}),
	lager:debug("Publish on ~p -> ~p", [S#state.game_channel, Msg]),
	eredis:q(S#state.publisher,
		["PUBLISH", S#state.game_channel, Msg]
		),
	{ok, ok, S};

handle_call(R, _S) -> 
	lager:error("game_controller received ~p", [R]),
	{remove_handler, bad_call, undefined}.

handle_info(stop, S) -> {stop, normal, S};

handle_info(_Info, S) -> {ok, S}.

terminate(_R, S) ->
	eredis:stop(S#state.publisher),
	ok.

code_change(_Old, S, _Extra) -> {ok, S}.

game_channels(GID) ->
	Ch1 = erlang:binary_to_atom(iolist_to_binary([?MODULE_STRING, GID]), utf8),
	Ch2 = erlang:binary_to_atom(iolist_to_binary([?MODULE_STRING, "_redis_", GID]), utf8),
	{Ch1, Ch2}.
