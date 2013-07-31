-module(tag_server_app).

-behaviour(application).
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 100).

start(normal, _StartArgs) ->
  Dispatch = cowboy_router:compile(routes()),
  lager:debug("Dispatch: ~p", [Dispatch]),
  Port = port(),
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
  {ok, _RefId} = cowboy:start_http(ts_websocket,
                                   ?C_ACCEPTORS,
                                   [{port, Port}],
                                   ProtoOpts
                                  ),
  tag_server_sup:start_link().

stop(_State) ->
  ok.

% Internal Functions

routes() ->
  [
    {'_', [
        {"/", ws_handler, []}
        ]}
    ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port2} = application:get_env(http_port),
      Port2;
    Port ->
      Port
  end.
