-module(cors).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	Origin = allow_origin(),
	R2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, PUT, PATCH, POST, OPTIONS">>, Req),
	R3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, R2),
	R4 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"Content-Type, Authorization, X-Requested-With">>, R3),
	R5 = cowboy_req:set_resp_header(<<"Access-Control-Max-Age">>, <<"1000">>, R4),
	{ok, R5, Env}.

allow_origin() ->
	% And this could probably be DRY'd into the tag_server_app
	Dom = case os:getenv("CHASE_DOMAIN") of
		false ->
			{ok, App} = application:get_application(whereis(tag_server_sup)),
			{ok, D2} = application:get_env(App, chase_domain),
			D2;
		D1 -> D1
	end,
	[<<"http://">>, Dom].
