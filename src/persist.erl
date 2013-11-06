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

% Key API
-export([
	del/1, exists/1
	]).

% Set API
-export([
	set/1,
	deset/1,
	add/2, pop/1
	%remove/2,
	%members/1, ismember/2, size/1,
	%range/3,
	%intersection/1, interstore/2,
	%union/1, unionstore/2
	]).

-define(DEFAULT_PREFIX, "tag").
-define(SEP, ":").

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

mod_name()->
	% Why does ?MODULE not just work?  Why global?
	global:whereis_name(?MODULE).

% Keys
del(K) ->
	K2 = if is_list(K) -> K; true -> [K] end,
	gen_server:call(mod_name(), {command, ["DEL" | K2]}).

exists(K)->
	gen_server:call(mod_name(), {command, ["EXISTS", K]}).

% Models
save(Prefix, Key, Value) ->
	gen_server:call(mod_name(),
		{save_value, affix(Prefix, Key), Value}
		).

save(Key, Value) -> save(?DEFAULT_PREFIX, Key, Value).

load(Prefix, Key) ->
	KB = affix(Prefix, Key),
	gen_server:call(mod_name(), {get_value, KB}).

load(Key) -> load(?DEFAULT_PREFIX, Key).

% Sets
-record(set, { key, values = [], persisted = false, dirty = true }).

set(K) ->
  #set{key=K}.

deset(Set) when is_record(Set, set)->
	if
		Set#set.persisted ->
			% remove it
			Set#set{persisted = false, dirty = true};
		true ->
			%nop
			{ok, Set}
	end.

add(V, S) ->
	V2 = if is_list(V) -> V; true -> [V] end,
	AddRes = gen_server:call(mod_name(),
		{command, ["SADD", S#set.key | V2]}),
	S2 = S#set{persisted = true,
			values = lists:append(V2, S#set.values)},
	lager:debug("Add Result ~p", [AddRes]),
	S2.

pop(S) ->
	V = gen_server:call(mod_name(),
		{command, ["SPOP", S#set.key]}),
	lager:debug("Pop returned ~p", [V]).

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

handle_call({command, List}, _From, Redis) ->
	lager:debug("Command ~p", [List]),
	{ok, Value} = eredis:q(Redis, List),
	{reply, Value, Redis};

handle_call(_Message, _From, Redis) ->
	{reply, error, Redis}.

handle_cast(_Message, Redis) -> {noreply, Redis}.
handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.
