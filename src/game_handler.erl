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
		game = null
		}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	{ok, P} = profile:find_by_request(Req),

	{GID, _R} = cowboy_req:binding(game_id, Req),
	G = case game:find(GID) of
		undefined -> null;
		G2 -> G2
	end,

	{ok, Req, #state{game = G, observer = P}}.

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
