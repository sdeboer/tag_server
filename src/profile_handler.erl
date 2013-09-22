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
	UUID = sessions:uuid(Req),
	Observer = profile:find_or_create_by_session(UUID),
	OID = profile:id(Observer),
	lager:debug("OID ~p", [OID]),
	View = case cowboy_req:binding(profile_id, Req, undefined) of
		{undefined, _Req} ->
			Observer;
		{ProfileId, _Req} when OID == ProfileId ->
			Observer;
		{ProfileId, _Req} ->
			lager:debug("Lookup ~p", [ProfileId]),
			profile:find(ProfileId)
	end,

	S = #state{ viewing = View, observer = Observer },

	{ok, Req, S}.

content_types_provided(Req, State) ->
	{[
			{<<"application/json">>, to_json},
			{<<"text/html">>, to_json},
			{<<"text/plain">>, to_json}
			], Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PATCH">>, <<"PUT">>],
		Req, State}.

content_types_accepted(Req, State) ->
	{[
			{{<<"application">>, <<"json">>, []}, alter_profile}
			], Req, State}.

to_json(Req, S) ->
	case S#state.viewing of
		undefined ->
			Err = [{error, <<"not_found">>}],
			{halt, error_response(404, Err, Req), S};

		View ->
			Json = profile:to_json(View),
			% just showing the OID to show we can do it.
			% Json = jiffy:encode({[{oid, list_to_binary(OID)}]}),
			Resp = case cowboy_req:qs_val(<<"jsonp">>, Req) of
				{undefined, _Req2} -> Json;
				{Fn, _Req2} ->
					[Fn, <<"(">>, Json, <<");">>]
			end,
			{Resp, Req, S}
	end.

alter_profile(Req, S) ->
	lager:debug("altering ~p", [Req]),
	case S#state.viewing of
		undefined ->
			Json = jiffy:encode({[{alter_profile, <<"test">>}]}),
			{Json, Req, S};

		_View ->
			Err = [{error, <<"not_allowed">>}],
			{halt, error_response(405, Err, Req), S}
	end.

error_response(Code, Resp, Req) ->
	{ok, Req2} = cowboy_req:reply(
			Code,
			[{<<"content-type">>, <<"application/json">>}],
			jiffy:encode({Resp}),
			Req),
	Req2.
