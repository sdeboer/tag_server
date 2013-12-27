-module(pubsub).
-behaviour(gen_server).

-export([start_link/1]).

-export([
	init/1, terminate/2,
	handle_call/3, handle_cast/2, handle_info/2,
	code_change/3
	]).

-export([
	subscribe/2,
	publish/3,
	ack/1,
	stop/1
	]).

-record(state, {pub, sub}).

start_link(Proc) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Proc, []).

subscribe(PS, Ch) ->
	gen_server:cast(PS, {subscribe, Ch}).

publish(PS, Ch, Msg) ->
	gen_server:cast(PS, {publish, Ch, Msg}).

stop(PS) ->
	gen_server:cast(PS, stop).

init(Proc) ->
	{ok, Pub} = eredis:start_link(),
	{ok, Sub} = eredis_sub:start_link(),
	eredis_sub:controlling_process(Sub, Proc),
	{ok, #state{pub = Pub, sub = Sub}}.

ack(PS) ->
	gen_server:cast(PS, ack_message).

terminate(_Reason, S) ->
	eredis:stop(S#state.pub),
	eredis_sub:stop(S#state.sub),
	ok.

handle_call(_Msg, _From, S) -> {reply, ok, S}.

handle_cast({publish, Ch, Msg}, S) ->
	eredis:q(S#state.pub, ["PUBLISH", Ch | Msg]),
	{noreply, S};

handle_cast({subscribe, Ch}, S) ->
	eredis_sub:subscribe(S#state.sub, Ch),
	{noreply, S};

handle_cast(ack_message, S) ->
	eredis_sub:ack_message(S#state.pub);

handle_cast(stop, S) ->
	{stop, normal, S}.

handle_info(_Msg, S) -> {noreply, S}.

code_change(_Old, S, _Extra) -> {ok, S}.
