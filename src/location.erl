-module(location).

-export([
	update/3,
	coords/2, coords/3
	]).

-record(coords, {
		lat, long,
		x = 0, y = 0, z = 0,
		scale = [1, 0, 1]
		}).

-define(PREFIX, ?MODULE_STRING).
-define(COORDS_PREFIX, [?PREFIX, <<"c">>]).

update(Hash, Key, Data) ->
	Alt = case proplists:get_value(<<"altitude">>, Data, null) of
		null -> 0;
		A1 -> A1
	end,

	Acc = case proplists:get_value(<<"accuracy">>, Data, null) of
		null -> 0;
		AC -> AC
	end,

	Current = coords(Hash, Key),

	{Res, C} = move(Current,
			proplists:get_value(<<"latitude">>, Data),
			proplists:get_value(<<"longitude">>, Data),
			Alt,
			Acc
			),

	case Res of
		change -> coords(Hash, Key, C);
		nochange -> nop
	end,

	[{x, C#coords.x}, {y, C#coords.y}, {z, C#coords.z}].

move(undefined, La2, Lo2, _Alt, _Acc) ->
	C = #coords{lat = La2, long = Lo2},
	{change, C};

move(C, La2, Lo2, _Alt, Acc) ->
	La1 = C#coords.lat,
	Lo1 = C#coords.long,

	if
		La1 == La2 andalso Lo1 == Lo2 ->
			{nochange, C};

		true ->
			{R, W} = geo:vector({La1, Lo1}, {La2, Lo2}),

			if
				R >= Acc ->
					X = C#coords.x,
					Y = C#coords.y,
					[ Scale, _, _ ] = C#coords.scale,

					R2 = R * Scale,
					X1 = X + geo:x(R2, W),
					Y1 = Y + geo:y(R2, W),

					C1 = C#coords{x = X1,
							y = Y1,
							lat = La2,
							long = Lo2},

					{change, C1};

				true -> {nochange, C}
			end
	end.

coords(H, K) ->
	case persist:hash_get([?COORDS_PREFIX, H], K) of
		undefined -> undefined;
		V ->
			{PL} = jiffy:decode(V),
			proplist_to_coords(PL)
	end.

coords(H, K, C) when is_record(C, coords) ->
	PL = coords_to_proplist(C),
	lager:debug("coords for ~p -> ~p", [K, PL]),
	C1 = jiffy:encode({PL}),
	<<"1">> = persist:hash_set([?COORDS_PREFIX, H], K, C1),
	C;

coords(H, K, Pos) ->
	coords(H, K, #coords{
									x = proplists:get_value(x, Pos),
									y = proplists:get_value(y, Pos),
									z = proplists:get_value(z, Pos)
								 }),
	Pos.

coords_to_proplist(#coords{} = Rec) ->
	lists:zip(
		record_info(fields, coords), tl(tuple_to_list(Rec))
		).

% It would be nice if this could be programmatic.  But since
% records are compile time it isn't straight forward.  The
% way to do it is through creating the appropriate tuple object
% by underlying knowledge rather than using any supplied record
% functionality.
proplist_to_coords(PL) ->
	F = fun(X)->
			case proplists:get_value(X, PL) of
				<<"undefined">> -> undefined;
				V -> V
			end
	end,
	#coords{
		lat = F(<<"lat">>),
		long = F(<<"long">>),
		x = F(<<"x">>),
		y = F(<<"y">>),
		z = F(<<"z">>),
		scale = F(<<"scale">>)
		}.
