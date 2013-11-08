-module(game).

-export([
	find/1,
	create/2,
	list/1, list/2, list/3
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
	case persist:load(?META_PREFIX, GID) of
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
	ok = persist:save(?META_PREFIX, GID, M),
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

list(Player) -> list(undefined, undefined, Player).

list(GT, GS) -> list(GT, GS, undefined).

list(undefined, undefined, undefined) -> [];

list(undefined, undefined, Player) ->
	lists:map(find, player_gids(Player));

list(undefined, GS, P) ->
	Gids = persist:members(persist:set([?STATE_PREFIX, GS])),
	filter_player(Gids, P);

list(GT, undefined, P) ->
	Gids = persist:members(persist:set([?TYPE_PREFIX, GT])),
	filter_player(Gids, P);

list(GT, GS, P) ->
	Gids = persist:intersection(
			[
				persist:set([?STATE_PREFIX, GS]),
				persist:state([?TYPE_PREFIX, GT])
				]
			),
	filter_player(Gids, P).

to_json(G) -> jiffy:encode(G).

% Private

player_gids(P) ->
	Pid = profile:id(P),
	persist:members(persist:set([?PLAYER_GAME_PREFIX, Pid])).

filter_player(Gids, undefined) ->
	lists:map(find, Gids);

filter_player(Gids, P) ->
	PGids = player_gids(P),
	MG = fun(G) ->
			C = lists:member(G, PGids),
			if C -> {true, find(G)};
				true -> false
			end
	end,
	lists:filtermap(MG, Gids).
