-module(game).

-export([
	find/1,
	create/2,
	list/2, list/3
	]).

-export([
	id/1,
	type/1, state/1,
	to_json/1
	]).

-record(game, {
		id = undefined,
		label = undefined,
		state = undefined,
		players = undefined,
		owner = undefined,
		type = undefined
		}).

-define(PREFIX, ?MODULE).

find(GID) ->
	case persist:load(?PREFIX, GID) of
		undefined -> undefined;
		Data -> binary_to_term(Data)
	end.

create(GameType, Owner) ->
	GID = list_to_binary(uuid:to_string(uuid:uuid4())),
	PID = profile:id(Owner),
	lager:debug("Creating Game for ~p : ~p : ~p", [GID, GameType, PID]),
	G = #game{
			id = GID,
			players = [PID],
			owner = PID,
			state = open,
			type = GameType
			},
	% TODO need a start_at
	ok = persist:save(?PREFIX, GID, G),
	G.

id(G) -> G#game.id.

type(G) -> G#game.type.

state(G) -> G#game.state.

list(GT, GS) ->
	[].

list(GT, GS, Player) ->
	[].

to_json(G) -> jiffy:encode(G).
