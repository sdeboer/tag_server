-module(profile_handler).

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
	alter_profile/2
	]).

-record(state, {
		viewing = null,
		observer = null
		}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	{ok, SID} = sessions:uuid(Req),
	{ok, Observer} = profile:find_or_create_by_session(SID),
	OID = profile:id(Observer),
	View = case cowboy_req:binding(profile_id, Req) of
		{undefined, _Req} ->
			undefined;
		{ProfileId, _Req} when OID == ProfileId ->
			Observer;
		{ProfileId, _Req} ->
			profile:find(ProfileId)
	end,

	S = #state{ viewing = View, observer = Observer },

	{ok, Req, S}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PATCH">>, <<"PUT">>],
		Req, State}.

content_types_accepted(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, alter_profile}
			], Req, State}.

content_types_provided(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, '*'}, to_json},
			{<<"text/html">>, to_json},
			{<<"text/plain">>, to_json}
			], Req, State}.

alter_profile(Req, S) ->
	P = S#state.observer,
	case cowboy_req:body(Req) of
		{ok, Body, R2} ->
			P2 = profile:update(jiffy:decode(Body), P),
			json_handler:construct_response(profile:to_json(P2), R2, S);
		{error, Reason} ->
			json_handler:encode_response({error, Reason}, Req, S, 400)
	end.

to_json(Req, S) ->
	Prof = case S#state.viewing of
		undefined -> S#state.observer;
		P -> P
	end,

	Json = profile:to_json(Prof),

	json_handler:return_json(Json, Req, S).

