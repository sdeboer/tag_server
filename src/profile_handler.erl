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
	SID = sessions:uuid(Req),
	Observer = profile:find_or_create_by_session(SID),
	OID = profile:id(Observer),
	View = case cowboy_req:binding(profile_id, Req, undefined) of
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

to_json(Req, S) ->
	lager:debug("TO"),
	Prof = case S#state.viewing of
		undefined -> S#state.observer;
		P -> P
	end,

	Json = profile:to_json(Prof),

	return_json(Json, Req, S).

return_json(Json, Req, S) ->
	Resp = case cowboy_req:qs_val(<<"jsonp">>, Req) of
		{undefined, _Req2} -> Json;
		{Fn, _Req2} ->
			[Fn, <<"(">>, Json, <<");">>]
	end,
	{Resp, Req, S}.

alter_profile(Req, S) ->
	P = S#state.observer,
	lager:debug("AP"),
	case cowboy_req:body(Req) of
		{ok, Body, R2} ->
			P2 = profile:update(jiffy:decode(Body), P),
			construct_response(profile:to_json(P2), R2);
		{error, Reason} ->
			encode_response(400, {error, Reason}, Req)
	end.

encode_response(Code, Resp, Req) ->
	construct_response(Code, jiffy:encode(Resp), Req).

construct_response(Json, Req) -> 
	construct_response(200, Json, Req).

construct_response(Code, Json, Req) ->
	{ok, R2} = cowboy_req:reply(
			Code,
			[{<<"content-type">>, <<"application/json">>}],
			Json,
			Req),
	R2.
