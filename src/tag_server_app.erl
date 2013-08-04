-module(tag_server_app).

-behaviour(application).
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 100).

start(normal, _StartArgs) ->
  PortOpts = [{port, port()}],
  ProtoOpts = [
      {env, [{dispatch, routes()}]},
      {middlewares, [sessions, cowboy_router, cowboy_handler]}
      ],
  {ok, _RefId} = cowboy:start_http(http, ?C_ACCEPTORS, PortOpts, ProtoOpts),
  tag_server_sup:start_link().

stop(_State) ->
  ok.

% Internal Functions

routes() ->
  cowboy_router:compile(
    [{'_',
      [
          {"/", toppage_handler, []},
          {"/profile/[:profile_id]", profile_handler, []},
          {"/ws", ws_handler, []},
          {"/static/[...]", cowboy_static, [
              {directory, {priv_dir, tag_server, [<<"static">>]}},
              {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
              ]}
          ]
     }]
    ).

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port2} = application:get_env(http_port),
      Port2;
    Port ->
      Port
  end.
