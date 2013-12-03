-module(game_list_handler).

-export([init/3]).

% Rest Standards
-export([
	rest_init/2,
	content_types_provided/2, content_types_accepted/2,
	allowed_methods/2
	]).

% Callbacks
-export([
	to_json/2,
	create_game/2
	]).

-record(state, { list }).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	L = list_games(Req),
	{ok, Req, #state{list = L}}.

list_games(Req) ->
	GS = case cowboy_req:qs_val(<<"game_state">>, Req) of
		{undefined, _R} -> [<<"open">>];
		{<<"any">>, _R} -> [];
		{S, _R} -> [S]
	end,

	GT = case cowboy_req:qs_val(<<"game_type">>, Req) of
		{undefined, _R2} -> [<<"robot">>];
		{GT1, _R2} ->
			% need to separate commas
			[GT1]
	end,

	case cowboy_req:qs_val(<<"mine">>, Req) of
		{<<"true">>, _R3} -> 
			{ok, P} = profile:find_by_request(Req),
			game:list(GT, GS, [profile:id(P)]);
		{_V, _R3} -> 
			game:list(GT, GS)
	end.

allowed_methods(Req, State) ->
	{ [<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, to_json},
			{<<"text/html">>, to_json},
			{<<"text/plain">>, to_json}
			], Req, State}.

content_types_accepted(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, create_game}
			], Req, State}.

create_game(Req, State) ->
	case cowboy_req:body(Req) of
		{ok, B, R2} ->
			{Body} = jiffy:decode(B),
			Gt = proplists:get_value(<<"game_type">>, Body),
			{ok, P} = profile:find_by_request(Req),
			Gm = game:create(Gt, P),
			json_handler:construct_response(game:to_json(Gm), R2, State);
		{error, Reason} ->
			json_handler:encode_response({error, Reason}, Req, State, 400)
	end.

to_json(Req, S) ->
	Json = jiffy:encode(S#state.list),
	json_handler:return_json(Json, Req, S).
