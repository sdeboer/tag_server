-module(tag_server_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = [{'_', [
          {'_', ws_handler, []}
          ]}],
  cowboy:start_listener(ts_websocket, 100,
                        cowboy_tcp_transport, [{port, 10100}],
                        cowboy_http_protocol, [{dispatch, Dispatch}]
                       ),
  tag_server_sup:start_link().

stop(_State) ->
  ok.
