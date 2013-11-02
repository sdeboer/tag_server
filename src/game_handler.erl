-module(game_handler).

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
	alter_game/2
	]).

-record(state, {
		observer = null,
		game = null,
		list = null
		}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	{ok, P} = profile:find_by_request(Req),
	S = #state{observer = P},

	S2 = case cowboy_req:binding(game_id, Req) of
		{undefined, R2} ->
			list_games(R2, S);
		{GID, _R} -> 
			S#state{game = game:find(GID)}
	end,

	{ok, Req, S2}.

list_games(Req, S) ->
	GS = case cowboy_req:qs_val(game_state, Req) of
		{undefined, _R} -> any;
		{<<"open">>, _R} -> open;
		{<<"running">>, _R} -> running;
		{<<"finished">>, _R} -> finished;
		{<<"any">>, _R} -> any
	end,

	GT = case cowboy_req:qs_val(game_type, Req) of
		{undefined, _R2} -> any;
		{_GT, _R2} -> robot
	end,

	List = case cowboy_req:qs_val(mine, Req) of
		{<<"true">>, _R3} -> 
			game:list(GT, GS, S#state.observer);
		{_V, _R3} -> 
			game:list(GT, GS)
	end,

	S#state{list = List}.

allowed_methods(Req, State) ->
	{
		[<<"GET">>, <<"POST">>, <<"PUT">>],
		Req, State}.

content_types_accepted(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, alter_game}
			], Req, State}.

content_types_provided(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, to_json},
			{<<"text/html">>, to_json},
			{<<"text/plain">>, to_json}
			], Req, State}.

alter_game(Req, S) ->
	{halt, Req, S}.

to_json(Req, S) ->
	Json = game:to_json(S#state.game),
	json_handler:return_json(Json, Req, S).
