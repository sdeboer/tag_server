-module(geo).

-export([
	distance/2,
	bearing/2,
	vector/2
	]).

-export([
	degtorad/1,
	radtodeg/1,
	sexagesimaltodecimal/3
	]).

-define(RADIUS, 6371.0).

vector({La1, Lo1, A1}, {La2, Lo2, A2}) ->
	Alt = (A1 + A2) / 2.0,
	vector({La1, Lo1}, {La2, Lo2}, ?RADIUS + Alt);

vector({La1, Lo1}, {La2, Lo2}) ->
	vector({La1, Lo1}, {La2, Lo2}, ?RADIUS).

vector({La1, Lo1}, {La2, Lo2}, R) ->
	RLa1 = degtorad(La1),
	RLo1 = degtorad(Lo1),
	RLa2 = degtorad(La2),
	RLo2 = degtorad(Lo2),

	DLong = RLo2 - RLo1,
	SLa1 = math:sin(RLa1),
	SLa2 = math:sin(RLa2),
	CLa1 = math:cos(RLa1),
	CLa2 = math:cos(RLa2),
	CDlong = math:cos(DLong),
	SDlong = math:sin(DLong),

	Dist = math:acos(
			(SLa1 * SLa2) +
			(CLa1 * CLa2 * CDlong) ) * R,

	Bearing = math:atan2(
			SDlong * CLa2,
			(CLa1 * SLa2) - (SLa1 * CLa2 * CDlong)),

	{Dist, Bearing}.

distance(A, B) ->
	element(1, vector(A, B)).

bearing(A, B) ->
	element(2, vector(A, B)).

degtorad(D) ->
	D * math:pi() / 180.

radtodeg(R) ->
	R * 180 / math:pi().

sexagesimaltodecimal(D, M, S) ->
	D + (M / 60.0) + (S / 3600.0).

