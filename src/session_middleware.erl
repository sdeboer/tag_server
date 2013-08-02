-module(session_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  case cowboy_req:cookie(<<"ts_session">>, Req, undefined) of
    {undefined, _} ->
      % FIXME this should really be a secure cookie of the nature
      % used within Rails with the secret key.
      UUID = uuid:to_string(uuid:uuid3(dns, "gvt.ws")),
      lager:info("SM : ~p", [UUID]),
      Req2 = cowboy_req:set_resp_cookie(<<"ts_session">>, UUID, [], Req),
      {ok, Req2, Env};
    {U2, _} ->
      % FIXME cont'd and then we need to confirm that we still
      % like this browser.
      lager:info("U2 : ~p", [U2]),
      {ok, Req, Env}
  end.
