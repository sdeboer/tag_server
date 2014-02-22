-module(pubsub_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([
	event_subscriber/1, stop_subscriber/1
	]).

-export([init/1]).

-define(CHILD(Id, M, Args),
	{
		Id,
		{M, start_link, [Args]},
		transient,
		5000,
		worker,
		[M]
		}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, { {one_for_one, 5, 10}, []} }.

stop_subscriber(Ch) ->
	{Ch1, Ch2} = game_controller:game_channels(Ch),
	supervisor:terminate_child(?MODULE, Ch2),
	supervisor:terminate_child(?MODULE, Ch1),
	supervisor:delete_child(?MODULE, Ch2),
	supervisor:delete_child(?MODULE, Ch1),
	ok.

event_subscriber(Ch) ->
	R = game_controller:game_channels(Ch),
	lager:debug("ES R ~p", [R]),
	{Ch1, Ch2} = R,
	Child = ?CHILD(Ch1, gen_event, {local, Ch1}),
	Event = supervisor:start_child(?MODULE, Child),

	case Event of
		{ok, Sub} ->
			% Brand new event channel, need to create the 
			% redis subscriber to go with it.

			{ok, _GS} = supervisor:start_child(?MODULE, ?CHILD(Ch2, game_subscriber, [Sub, Ch]) ),
			Sub;

		{error, {already_started, Sub}} -> Sub
	end.
