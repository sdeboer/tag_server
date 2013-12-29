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
	{ok, R} = eredis_sub:start_link(),
	eredis_sub:controlling_process(R, self()),
	eredis_sub:subscribe(R, [Ch]),
	{ok, Event}.

handle_info({message, _Ch, Msg, Sub}, Event) ->
	gen_event:notify(
		Event,
		jiffy:decode(Msg)
		),
	eredis_sub:ack_message(Sub),
	{noreply, Event};

handle_info({subscribed, _Ch, _Sub}, Event) ->
	{noreply, Event}.

terminate(_R, _E) -> ok.

handle_cast(_M, _S) -> {stop, bad_call, undefined}.

handle_call(_R, _F, _S) -> {stop, bad_call, undefined}.

code_change(_O, E, _Ex) -> {ok, E}.
