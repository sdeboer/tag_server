-module(sessions).

-behaviour(cowboy_middleware).

-export([execute/2]).

-export([uuid/1]).

execute(Req, Env) ->
	case cowboy_req:cookie(<<"ts_session">>, Req, undefined) of
		{undefined, _} ->
			% FIXME this should really be a secure cookie of the nature
			% used within Rails with the secret key.
			lager:debug("Within sessions:execute"),
			UUID = uuid:to_string(uuid:uuid3(dns, cowboy_req:host(Req))),
			Req2 = cowboy_req:set_resp_cookie(session_name(), UUID, [], Req),
			{ok, Req2, Env};
		{_UUID, _} ->
			% FIXME cont'd and then we need to confirm that we still
			% like this browser.
			{ok, Req, Env}
	end.

uuid(Req) ->
	{Val, _R2} = cowboy_req:cookie(session_name(), Req, undefined),
	Val.

session_name() ->
	% Is this expensive enough that it should be memoizing the
	% value?
	%
	% And this could probably be DRY'd into the tag_server_app
	{ok, App} = application:get_application(whereis(tag_server_sup)),
	case os:getenv("SESSION_COOKIE") of
		false ->
			{ok, Sc2} = application:get_env(App, session_cookie),
			Sc2;
		Sc ->
			Sc
	end.
