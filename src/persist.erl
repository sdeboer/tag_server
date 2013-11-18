-module(persist).
-behaviour(gen_server).

-export([start_link/0]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
	terminate/2, handle_info/2, 
	code_change/3, stop/1]).

% Model API
-export([
	save/2, 
	load/1
	]).

% Key API
-export([
	del/1, exists/1
	]).

% List API
-export([
	list/1,
	delist/1,
	push/2, pop/1, pop/2,
	unshift/2, shift/1, shift/2,
	items/1 %, hasitem/2, length/1,
	%range/3,
	]).

% Set API
-export([
	set/1, deset/1,
	add/2, remove/2,
	contains/2, members/1,
	cardinality/1, card/1,
	intersection/1, interstore/2,
	union/1, unionstore/2
	]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

mod_name()->
	% Why does ?MODULE not just work?  Why global?
	global:whereis_name(?MODULE).

% Keys
del(K) ->
	cmmd(["DEL", K]).

exists(K)->
	cmmd(["EXISTS", K]).

% Models
save(Key, Value) ->
	gen_server:call(mod_name(),
		{save_value, Key, Value}
		).

load(Key) ->
	gen_server:call(mod_name(), {get_value, Key}).

% Lists
-record(list, { key, persisted = false, dirty = true }).

list(K) -> #list{key = K}.

delist(S) when is_record(S, list) ->
	del(S#list.key),
	{ok, S#list{persisted = false, dirty = true}};

delist(K) -> 
	del(K),
	{ok, #list{key = K}}.

push([], S) -> S;

push(List, S) ->
	cmmd(["RPUSH", S#list.key | List]),
	S#list{ persisted = true }.

unshift([], L) -> L;

unshift(List, S) ->
	cmmd(["LPUSH", S#list.key | List]),
	S#list{ persisted = true }.

pop(L) ->
	% check undefined (empty list)
	V = cmmd(["LPOP", L#list.key]),
	{V, L}.

pop(N, L)->
	Vs = cmmd(["LRANGE", L#list.key, 0, N - 1]),
	Len = length(Vs),
	if Len > 0 -> 
			cmmd(["LREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

shift(S) ->
	% check undefined (empty list)
	V = cmmd(["RPOP", S#list.key]),
	{V, S}.

shift(N, L)->
	Vs = cmmd(["RRANGE", L#list.key, 0, N -1]),
	Len = length(Vs),
	if Len > 0 -> 
			cmmd(["RREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

items(S) ->
	V = cmmd(["LRANGE", S#list.key, 0, -1]),
	lager:debug("MEMBERS ~p", [V]),
	{V, S#list{persisted = true, dirty = false} }.

% Sets

-record(set, { key, persisted = false, dirty = true }).

set(K) -> {ok, #set{key = K}}.

deset(S) when is_record(S, set) ->
	del(S#set.key),
	{ok, S#set{persisted = false, dirty = true}};

deset(K) ->
	del(K),
	{ok, #set{key = K}}.

add(List, S) ->
	cmmd(["SADD", S#set.key | List]),
	{ok, S#set{ persisted = true }}.

remove(List, S) ->
	cmmd(["SREM", S#set.key | List]),
	{ok, S}.

contains(Item, S) ->
	R = case cmmd(["SISMEMBER", S#set.key, Item]) of
		<<"1">> -> true;
		<<"0">> -> false
	end,
	{R, S}.

members(S) ->
	{cmmd(["SMEMBERS", S#set.key]), S}.

cardinality(S) ->
	{cmmd(["SCARD", S#set.key]), S}.

card(S) -> cardinality(S).

keylist(SList) ->
	lists:map(fun(S) -> S#set.key end, SList).

intersection(SList) ->
	lager:debug("SL ~p", [SList]),
	R = cmmd(["SINTER" | keylist(SList)]),
	lager:debug("RR ~p", [R]),
	R.

interstore(Dest, SList) when is_record(Dest, set) ->
	cmmd(["SINTERSTORE", Dest#set.key, keylist(SList)]),
	Dest#set{persisted = true};

interstore(DK, SList) ->
	interstore(#set{key = DK}, SList).

union(SList) ->
	cmmd(["SUNION" | keylist(SList)]).

unionstore(Dest, SList) when is_record(Dest, set) ->
	cmmd(["SUNIONSTORE", Dest#set.key | keylist(SList)]),
	Dest#set{persisted = true};

unionstore(DK, SList) ->
	unionstore(#set{key = DK}, SList).

init([]) ->
	{ok, Redis} = eredis:start_link(),
	{ok, Redis}.

stop(_Pid) ->
	stop().

stop() ->
	gen_server:cast(mod_name(), stop).

cmmd(Arg) ->
	lager:debug("PE C ~p", [Arg]),
	gen_server:call(mod_name(), {cmmd, Arg}).

handle_call({get_value, Key}, _From, Redis) ->
	{ok, Value} = eredis:q(Redis, ["GET", Key]),
	{reply, Value, Redis};

handle_call({save_value, Key, Value}, _From, Redis) ->
	lager:debug("Saving ~p / ~p", [Key, Value]),
	{ok, <<"OK">>} = eredis:q(Redis, ["SET", Key, Value]),
	{reply, ok, Redis};

handle_call({cmmd, List}, _From, Redis) ->
	lager:debug("cmmd ~p", [List]),
	{ok, Value} = eredis:q(Redis, List),
	{reply, Value, Redis};

handle_call(_Message, _From, Redis) ->
	{reply, error, Redis}.

handle_cast(_Message, Redis) -> {noreply, Redis}.
handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.
