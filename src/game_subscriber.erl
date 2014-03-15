-module(game_subscriber).

-behaviour(gen_server).

-export([
	start_link/1
	]).

-export([
	init/1, terminate/2,
	handle_cast/2, handle_call/3, handle_info/2,
	code_change/3
	]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init([Event, Ch]) ->
	lager:debug("GS Start ~p : ~p", [Ch, self()]),
	{ok, R} = eredis_sub:start_link(),
	eredis_sub:controlling_process(R, self()),
	eredis_sub:subscribe(R, [Ch]),
	{ok, Event}.

handle_info({message, _Ch, Msg, Sub}, Event) ->
	lager:debug("GS received ~p", [Msg]),
	{M} = jiffy:decode(Msg),
	gen_event:notify(Event, M),
	eredis_sub:ack_message(Sub),
	{noreply, Event};

handle_info({subscribed, Ch, Sub}, Event) ->
	lager:debug("subscribed ~p", [Ch]),
	eredis_sub:ack_message(Sub),
	{noreply, Event};

handle_info({dropped, _N, Sub}, Event) ->
	lager:error("Game Subscriber dropped messages"),
	eredis_sub:ack_message(Sub),
	{noreply, Event};

handle_info({eredis_disconnected, Sub}, Event) ->
	lager:notice("Game subscriber disconnected"),
	eredis_sub:ack_message(Sub),
	{noreply, Event};

handle_info({eredis_connected, Sub}, Event) ->
	lager:notice("Game subscriber reconnected"),
	eredis_sub:ack_message(Sub),
	{noreply, Event}.

terminate(_R, _E) -> ok.

handle_cast(M, _S) -> 
	lager:debug("GS cast ~p", [M]),
	{stop, bad_call, undefined}.

handle_call(R, _F, _S) -> 
	lager:debug("GS call ~p", [R]),
	{stop, bad_call, undefined}.

code_change(_O, E, _Ex) -> {ok, E}.
