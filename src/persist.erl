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
	% remove/2,
	members/1 %, ismember/2, size/1,
	%range/3,
	%intersection/1, interstore/2,
	%union/1, unionstore/2
	]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

mod_name()->
	% Why does ?MODULE not just work?  Why global?
	global:whereis_name(?MODULE).

% Keys
del(K) ->
	gen_server:call(mod_name(), {command, ["DEL", K]}).

exists(K)->
	gen_server:call(mod_name(), {command, ["EXISTS", K]}).

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
	AddRes = command(["RPUSH", S#list.key | List]),
	S2 = S#list{ persisted = true },
	lager:debug("Add Result ~p", [AddRes]),
	S2.

unshift([], L) -> L;

unshift(List, S) ->
	AddRes = command(["LPUSH", S#list.key | List]),
	S2 = S#list{ persisted = true },
	lager:debug("Add Result ~p", [AddRes]),
	S2.

pop(L) ->
	% check undefined (empty list)
	V = command(["LPOP", L#list.key]),
	{V, L}.

pop(N, L)->
	Vs = command(["LRANGE", L#list.key, 0, N - 1]),
	Len = length(Vs),
	if Len > 0 -> 
			command(["LREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

shift(S) ->
	% check undefined (empty list)
	V = command(["RPOP", S#list.key]),
	{V, S}.

shift(N, L)->
	Vs = command(["RRANGE", L#list.key, 0, N -1]),
	Len = length(Vs),
	if Len > 0 -> 
			command(["RREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

members(S) ->
	V = command(["LRANGE", S#list.key, 0, -1]),
	lager:debug("MEMBERS ~p", [V]),
	{V, S#list{persisted = true, dirty = false} }.

init([]) ->
	{ok, Redis} = eredis:start_link(),
	{ok, Redis}.

stop(_Pid) ->
	stop().

stop() ->
	gen_server:cast(mod_name(), stop).

command(Arg) ->
	gen_server:call(mod_name(), {command, Arg}).

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
