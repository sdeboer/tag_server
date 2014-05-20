-module(sessions).

-behaviour(cowboy_middleware).

-export([execute/2]).

-export([uuid/1]).

execute(Req, Env) ->
	Name = session_name(),
	case cowboy_req:cookie(Name, Req) of
		{undefined, R} ->
			% FIXME this should really be a secure cookie of the nature
			% used within Rails with the secret key.
			UUID = uuid:to_string(uuid:uuid1()),
			Opts = [ {http_only, true} ],
			Req2 = cowboy_req:set_resp_cookie(Name, UUID, Opts, R),
			% To have the correct UUID through this initial request
			Req3 = cowboy_req:set_meta(Name, UUID, Req2),
			{ok, Req3, Env};

		{_UUID, R} ->
			% FIXME cont'd and then we need to confirm that we still
			% like this browser.
			{ok, R, Env}
	end.

uuid(Req) ->
	Name = session_name(),
	case cowboy_req:cookie(Name, Req, undefined) of
		{undefined, R} -> 
			% The first request will not have a cookie yet
			{U, _} = cowboy_req:meta(Name, R),
			U;
		{U, _} -> U
	end.

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
