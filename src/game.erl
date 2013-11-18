-module(game).

-export([
	find/1,
	create/2,
	list/1, list/2, list/3,
	all/3
	]).

-export([
	id/1,
	type/1, state/1, started/1,
	players/1, owner/1,
	to_json/1
	]).

-record(meta, {
		id,
		label,
		owner,
		state,
		type,
		started
		}).

-record(game, {
		players,
		meta
		}).

-define(PREFIX, ?MODULE_STRING).
-define(META_PREFIX, [?PREFIX, <<"m">>]).
-define(TYPE_PREFIX, [?PREFIX, <<"t">>]).
-define(PLAYERS_PREFIX, [?PREFIX, <<"p">>]).
-define(STATE_PREFIX, [?PREFIX, <<"s">>]).
-define(PLAYER_GAME_PREFIX, [?PREFIX, <<"pg">>]).

find(GID) ->
	case persist:load([?META_PREFIX, GID]) of
		undefined -> undefined;
		Data ->
			M = binary_to_term(Data),
			#game{
				meta = M,
				players = persist:set([?PLAYERS_PREFIX, M#meta.id])
				}
	end.

create(GameType, Owner) ->
	GID = list_to_binary(uuid:to_string(uuid:uuid4())),
	PID = profile:id(Owner),
	lager:debug("Creating Game for ~p : ~p : ~p", [GID, GameType, PID]),
	State = open,
	M = #meta{
			id = GID,
			owner = PID,
			state = State,
			label = [<<"game for : ">>, profile:handle(Owner)],
			started = calendar:universal_time(),
			type = GameType
			},
	ok = persist:save([?META_PREFIX, GID], M),
	P1 = persist:set([?PLAYERS_PREFIX, GID]),
	P2 = persist:add([PID], P1),
	G = #game{meta = M, players = P2},

	Pl1 = persist:set([?PLAYER_GAME_PREFIX, PID]),
	persist:add([GID], Pl1),

	T1 = persist:set([?TYPE_PREFIX, GameType]),
	persist:add([GID], T1),

	S1 = persist:set([?STATE_PREFIX, State]),
	persist:add([GID], S1),

	G.

id(G) -> G#game.meta#meta.id.

type(G) -> G#game.meta#meta.type.

state(G) -> G#game.meta#meta.state.

started(G) -> G#game.meta#meta.started.

players(G) ->
	PL = persist:members(G#game.players),
	MP = fun(Pid) -> player:find(Pid) end,
	lists:map(MP, PL).

owner(G) ->
	player:find(G#game.meta#meta.owner).

list(Pl) -> list([], [], Pl).

list(Tl, Sl) -> list(Tl, Sl, []).

list(Tl, Sl, Pl) ->
	persist:intersection(key_list(Tl, Sl, Pl)).

all(Tl, Sl, Pl) ->
	persist:union(key_list(Tl, Sl, Pl)).

key_list_to_set (Prefix, L)->
	Lp = fun(K) ->
			{ok, S} = persist:set([Prefix, K]),
			S
	end,
	lists:map(Lp, L).

key_list([], [], Pl) ->
	key_list([<<"ellipse">>], [], Pl);

key_list(Tl, Sl, Pl) ->
	lager:debug("kl ~p", [Tl]),
	Kl = [
		key_list_to_set(?TYPE_PREFIX, Tl),
		key_list_to_set(?STATE_PREFIX, Sl),
		key_list_to_set(?PLAYER_GAME_PREFIX, Pl)
		],
	lists:flatten(Kl).

to_json(G) -> jiffy:encode(G).
