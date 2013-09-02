-module(profile_handler).

-export([init/3]).

% Rest Standards
-export([
	rest_init/2,
	content_types_provided/2,
%content_types_accepted/2,
	allowed_methods/2
	]).

% Callbacks
-export([
	to_json/2, create_profile/2
	]).

init({tcp, http}, _Req, _Opts) ->
	lager:info("Upgrading to REST"),
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	lager:debug("REST Init"),
	UUID = sessions:uuid(Req),
	lager:debug("UUID ~p.", UUID),
	Observer = profile:find_or_create_by_session(UUID),
	OID = profile:id(Observer),
	View = case cowboy_req:binding(profile_id, Req) of
		undefined ->
			Observer;
		ProfileId when OID == ProfileId->
			Observer;
		ProfileId ->
			profile:find(ProfileId)
	end,

	State = [{profile, View}, {observer, Observer}],

	{ok, Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json},
		{<<"text/html">>, to_json},
		{<<"text/plain">>, to_json}
		], Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>],
		Req, State}.

%content_types_accepted(Req, State) ->
	%{[
			%{{<<"application">>, <<"x-www-form-urlencoded">>, []}, created_profile}],
		%Req, State}.

to_json(Req, State) ->
	{<<"[\"to_json\",1,2,3,4]">>, Req, State}.

create_profile(Req, State) ->
	{<<"[\"create_profile\",10,11,12,13]">>, Req, State}.
