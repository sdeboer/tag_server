-module(play_handler).

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
		game
		}).

websocket_init(_Transport, Req, _RouteOpts) ->
	{ok, SID} = sessions:uuid(Req),

	{GID, R2} = cowboy_req:binding(game_id, Req),
	Game = game:find(GID),

	case profile:find_by_session(SID) of
		{ok, Observer} -> 
			Sub = pubsub_sup:event_subscriber(GID),

			GCProps = [
					{game_id, GID},
					{profile_id, profile:id(Observer)},
					{websocket, self()}
					],

			ok = gen_event:add_sup_handler(Sub, {game_controller, GID}, GCProps),

			R3 = cowboy_req:compact(R2),

			{ok, R3, #state{
					observer = Observer,
					subscriber = Sub,
					game = Game
					}}

			;

		undefined -> {shutdown, Req}
	end.

% Called when text message arrives
websocket_handle({text, Msg}, Req, S) ->
	lager:debug("WS Rec: ~p", [Msg]),
	{Data} = jiffy:decode(Msg),

	WSID = proplists:get_value(<<"callback_id">>, Data),

	Ret = case proplists:get_value(<<"command">>, Data) of
		<<"geo">> ->
			{Coords} = proplists:get_value(<<"coords">>, Data),

			Loc = location:update(
					S#state.game,
					S#state.observer,
					Coords
					),

			gen_event:call(S#state.subscriber, {geo, Loc}),
			{ack, WSID};
		undefined ->
			{nop, WSID}
	end,

	{reply,
		{text, jiffy:encode({[Ret]})},
		Req, S};

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
