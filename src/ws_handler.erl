-module(ws_handler).

-behaviour(cowboy_websocket_handler).

% for cowboy_websocket_handler
-export([
  init/3,
  websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3
  ]).

init({tcp, http}, Req, _RouteOpts) ->
  lager:info("Request: ~p", [Req]),
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _RouteOpts) ->
  lager:info("New Client"),
  erlang:start_timer(1000, self(), <<"Hi!">>),
  {ok, Req, [{pears, <<"twe">>}]}.

% Called when text message arrives
websocket_handle({text, Msg}, Req, State) ->
  lager:info("Received: ~p", [State]),
  {reply,
   {text, <<"Responding to ", Msg/binary>>},
   Req, State};

% For any other type of content that gets sent
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(1000, self(), <<"May I have anothe?">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
