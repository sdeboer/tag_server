-module(sessions).

-behaviour(cowboy_middleware).

-export([execute/2]).

-export([uuid/1]).

execute(Req, Env) ->
	Name = session_name(),
	case cowboy_req:cookie(Name, Req) of
		{undefined, _} ->
			% FIXME this should really be a secure cookie of the nature
			% used within Rails with the secret key.
			{Host, _} = cowboy_req:host(Req),
			UUID = uuid:to_string(uuid:uuid3(dns, Host)),
			Opts = [{domain, "localhost"}],
			Req2 = cowboy_req:set_resp_cookie(Name, UUID, Opts, Req),
			{ok, Req2, Env};
		{_UUID, _} ->
			% FIXME cont'd and then we need to confirm that we still
			% like this browser.
			{ok, Req, Env}
	end.

uuid(Req) ->
	{Val, _} = cowboy_req:cookie(session_name(), Req, undefined),
	Val.

session_name() ->
	% Is this expensive enough that it should be memoizing the
	% value?  Probably not since we want to allow erlang to change
	% it when it reloads on the fly.
	%
	% And this could probably be DRY'd into the tag_server_app
	case os:getenv("SESSION_COOKIE") of
		false ->
			{ok, App} = application:get_application(whereis(tag_server_sup)),
			{ok, Sc2} = application:get_env(App, session_cookie),
			Sc2;
		Sc ->
			Sc
	end.
