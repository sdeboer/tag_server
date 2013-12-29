-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([
	init/3,
	websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3
	]).

init({tcp, http}, _Req, _RouteOpts) ->
	{upgrade, protocol, cowboy_websocket}.

-record(state, {
		observer,
		subscriber,
		game_id
		}).

websocket_init(_Transport, Req, _RouteOpts) ->
	% erlang:start_timer(1000, self(), <<"Hi!">>),
	{ok, SID} = sessions:uuid(Req),
	Observer = case profile:find_by_session(SID) of
		{ok, O} -> O;
		undefined ->
			%self() ! [{send, null}, {type, <<"profile">>}, {message, <<"needed">>}],
			undefined
	end,

	GID = <<"GID">>,
	Sub = pubsub_sup:event_subscriber(GID),
	GCProps = [
			{game_id, GID},
			{profile_id, profile:id(Observer)},
			{websocket, self()}
			],

	ok = gen_event:add_sup_handler(Sub, {game_controller, GID}, GCProps),
	Req2 = cowboy_req:compact(Req),
	{ok, Req2, #state{
			observer = Observer,
			subscriber = Sub,
			game_id = GID}}.

% Called when text message arrives
websocket_handle({text, Msg}, Req, State) ->
	lager:debug("WS Rec: ~p", [Msg]),
	{reply,
		{text, <<Msg/binary>>},
		Req, State};

% For any other type of content that gets sent
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	% erlang:start_timer(1000, self(), <<"May I have blag??">>),
	{reply, {text, Msg}, Req, State};

websocket_info({send, Ref, Msg}, Req, State) ->
	{reply, {text, jiffy:encode({Ref, Msg})}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
