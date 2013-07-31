-module(ws_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

% for cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% for cowboy_http_websocket_handler
-export([
  websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
  ]).

init(_Transport, Req, _Opts) ->
  lager:debug("Request: ~p", [Req]),
  {upgrade, protocol, cowboy_http_websocket}.

% Since we upgraded to WS in init, should never get here
handle(Req, State) ->
  lager:warning("Unexpected request: ~p", [Req]),
  {ok, Req2} = cowboy_http_req:reply(404, [
        {'Content-Type', <<"text/html">>}
        ]),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Called for every new websocket connection
websocket_init(_Transport, Req, []) ->
  lager:debug("New Client"),
  Req2 = cowboy_http_req:compact(Req),
  {ok, Req2, undefined, hibernate}.

% Called when text message arrives
websocket_handle({text, Msg}, Req, State) ->
  lager:debug("Received: ~p", [Msg]),
  {reply,
   {text, <<"Responding to ", Msg/binary>>},
   Req, State, hibernate
  };

% For any other type of content that gets sent
websocket_handle(_Any, Req, State) ->
  {ok, Req, State}.

% Other message from the system
websocket_info(_Info, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.



