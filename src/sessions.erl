-module(sessions).

-behaviour(cowboy_middleware).

-export([execute/2]).

-export([uuid/1]).

execute(Req, Env) ->
  case cowboy_req:cookie(<<"ts_session">>, Req, undefined) of
    {undefined, _} ->
      % FIXME this should really be a secure cookie of the nature
      % used within Rails with the secret key.
      UUID = uuid:to_string(uuid:uuid3(dns, cowboy_req:host(Req))),
      Req2 = cowboy_req:set_resp_cookie(session_name(), UUID, [], Req),
      {ok, Req2, Env};
    {_UUID, _} ->
      % FIXME cont'd and then we need to confirm that we still
      % like this browser.
      {ok, Req, Env}
  end.

uuid(Req) ->
  cowboy_req:cookie(session_name(), Req).

session_name() ->
  % Is this expensive enough that it should be memoizing the
  % value?
  case os:getenv("SESSION_COOKIE") of
    false ->
      {ok, Sc2} = application:get_env(session_cookie),
      Sc2;
    Sc ->
      Sc
  end.
