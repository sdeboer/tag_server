-module(persist).
-behaviour(gen_server).

-export([start_link/0]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
	terminate/2, handle_info/2, 
	code_change/3, stop/1]).

% Model API
-export([save/2, save/3,
	load/1, load/2]).

-define(DEFAULT_PREFIX, "tag").
-define(SEP, ":").

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

mod_name()->
	% Why does ?MODULE not just work?  Why global?
	global:whereis_name(?MODULE).

save(Prefix, Key, Value) ->
	gen_server:call(mod_name(),
		{save_value, affix(Prefix, Key), Value}
		).

save(Key, Value) -> save(?DEFAULT_PREFIX, Key, Value).

load(Prefix, Key) ->
	KB = affix(Prefix, Key),
	gen_server:call(mod_name(), {get_value, KB}).

load(Key) -> load(?DEFAULT_PREFIX, Key).

affix(Prefix, Key) -> [Prefix, ?SEP, Key].

init([]) ->
	{ok, Redis} = eredis:start_link(),
	{ok, Redis}.

stop(_Pid) ->
	stop().

stop() ->
	gen_server:cast(mod_name(), stop).

handle_call({get_value, Key}, _From, Redis) ->
	{ok, Value} = eredis:q(Redis, ["GET", Key]),
	{reply, Value, Redis};

handle_call({save_value, Key, Value}, _From, Redis) ->
	lager:debug("Saving ~p / ~p", [Key, Value]),
	{ok, <<"OK">>} = eredis:q(Redis, ["SET", Key, Value]),
	{reply, ok, Redis};

handle_call(_Message, _From, Redis) ->
	{reply, error, Redis}.

handle_cast(_Message, Redis) -> {noreply, Redis}.
handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.
