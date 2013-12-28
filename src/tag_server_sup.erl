-module(tag_server_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type),
	{I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUP(I),
	{I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [
			?CHILD(persist, worker),
			?SUP(pubsub_sup),
			?SUP(game_controller_sup)
			],
	{ok, { {one_for_one, 5, 10}, Children} }.
