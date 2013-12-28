-module(game_controller_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MFA = {game_controller, start_link, []},
	Child = {game_controller,
			MFA,
			transient, 5, worker, 
			[game_controller, pubsub]
			},
	Sup = {
			{simple_one_for_one, 5, 10},
			[ Child ]
			},
	{ok, Sup}.
