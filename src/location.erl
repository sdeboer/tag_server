-module(location).

-export([
	update/3
	]).

-record(coords, {
		lat, long, % last one
		x, y, z,
		scale = {1, 0, 1}
		}).

update(Game, Profile, Data) ->
	Alt = case proplists:get_value(<<"altitude">>, Data, null) of
		null -> 0;
		A1 -> A1
	end,

	Acc = case proplists:get_value(<<"accuracy">>, Data, null) of
		null -> 0;
		AC -> AC
	end,

	Current = game:coords(Game, Profile),
	Coords = move(Current,
			proplists:get_value(<<"latitude">>, Data),
			proplists:get_value(<<"longitude">>, Data),
			Alt,
			Acc
			),

	case Coords of
		undefined -> undefined;
		C1 ->
			game:coords(Game, Profile, C1),
			C1
	end.

move(C, La2, Lo2, _Alt, Acc) ->
	La1 = C#coords.lat,
	Lo1 = C#coords.long,

	{R, W} = geo:vector({La1, Lo1}, {La2, Lo2}),
	if
		R >= Acc ->
			{Scale, _Dw, _AltS} = C#coords.scale,
			R2 = R * Scale,
			X = C#coords.x + geo:x(R2, W),
			Y = C#coords.y + geo:y(R2, W),
			C#coords{x = X, y = Y};
		true -> undefined
	end.


