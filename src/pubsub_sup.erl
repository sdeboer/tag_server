-module(pubsub_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MFA = {pubsub, start_link, []},
	Child = {pubsub,
			MFA,
			transient, 5, worker, 
			[pubsub]
			},
	Sup = {
			{simple_one_for_one, 5, 10},
			[ Child ]
			},
	{ok, Sup}.
