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
	to_json/1,
	finish/1
	]).

-record(meta, {
		id,
		label,
		owner,
		state,
		type,
		started,
		finished = undefined
		}).

-record(game, {
		players = [],
		meta,
		spiralPid = undefined
		}).

-define(PREFIX, ?MODULE_STRING).
-define(META_PREFIX, [?PREFIX, <<"m">>]).
-define(TYPE_PREFIX, [?PREFIX, <<"t">>]).
-define(PLAYERS_PREFIX, [?PREFIX, <<"p">>]).
-define(STATE_PREFIX, [?PREFIX, <<"s">>]).
-define(PLAYER_GAME_PREFIX, [?PREFIX, <<"pg">>]).

-define(R2P(Record),
	record_to_proplist(#Record{} = Rec) ->
	lists:zip(
	record_info(fields, Record), tl(tuple_to_list(Rec))
	)).

?R2P(meta).

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
	State = <<"open">>,
	M = #meta{
			id = GID,
			owner = PID,
			state = State,
			label = [<<"game for : ">>, profile:handle(Owner)],
			started = calendar:universal_time(),
			type = GameType
			},

	G = #game{meta = M, players = [PID]},

	{ok, P1} = persist:set([?PLAYERS_PREFIX, GID]),
	persist:add([PID], P1),

	{ok, Pl1} = persist:set([?PLAYER_GAME_PREFIX, PID]),
	persist:add([GID], Pl1),

	{ok, T1} = persist:set([?TYPE_PREFIX, GameType]),
	persist:add([GID], T1),

	{ok, S1} = persist:set([?STATE_PREFIX, State]),
	persist:add([GID], S1),

	G2 = type_init(G),
	save_meta(G2).

id(G) -> G#game.meta#meta.id.

type(G) -> G#game.meta#meta.type.

state(G) -> G#game.meta#meta.state.

started(G) -> G#game.meta#meta.started.

finish(G) ->
	case G#game.spiralPid of
		undefined -> nop;
		SPid -> gen_server:cast(SPid, stop)
	end,
	G2 = G#game.meta#meta{finished = calender:universal_time()},
	save_meta(G2).

save_meta(G) ->
	ok = persist:save([?META_PREFIX, id(G)], G#game.meta),
	G.

players(G) ->
	PL = persist:members(G#game.players),
	MP = fun(Pid) -> player:find(Pid) end,
	lists:map(MP, PL).

type_init(G) ->
	case game:type(G) of
		0 -> % Robot
			P1 = profile:create(),
			P2 = profile:handle(<<"Robot">>, P1),
			P3 = profile:save(P2),
			PID = profile:id(P3),
			% TODO attach this to a Sup
			% Save robot into Game and remove at finish
			{ok, SRPID} = spiral_robot:start_link([{game, G}, {profile_id, PID}]),
			G#game{spiralPid = SRPID};
		true -> G
	end.

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
	Kl = [
		key_list_to_set(?TYPE_PREFIX, Tl),
		key_list_to_set(?STATE_PREFIX, Sl),
		key_list_to_set(?PLAYER_GAME_PREFIX, Pl)
		],
	lists:flatten(Kl).

to_json(G) ->
	M = G#game.meta,
	M0 = M#meta{
			label = lists:flatten(M#meta.label),
			started = lists:flatten(
				[
					tuple_to_list(element(1, M#meta.started)),
					tuple_to_list(element(2, M#meta.started))
					]
				)},
	Data = [{players, G#game.players} | record_to_proplist(M0)],
	jiffy:encode({Data}).
