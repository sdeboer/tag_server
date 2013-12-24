-module(spiral_robot).
-behaviour(gen_server).

-export([ start_link/1 ]).

-export([
	init/1, terminate/2,
	handle_info/2, handle_call/3, handle_cast/2,
	code_change/3
	]).

-record(details, {
		game,
		profile_id,
		start,
		current,
		spiral,
		speed,  % km/ microsecond
		spiralling,
		stamp
		}).

% How close before collision
% TODO what about overshooting?
-define(TOLERANCE, 5).

% seconds between position updates
-define(DELTA, 5000).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
	% Defaults:
	% travel at 2 m/s
	% start at Guelph
	% go north approximately 500m before spiral
	St = proplists:get_value(start, Args, {0, 0, 334}),
	SF = proplists:get_value(spiral, Args, {500, 0, 334}),
	Sp = proplists:get_value(speed, Args, 2.0) / 1000000.0,
	S = #details{
			game = proplists:get_value(game, Args),
			profile_id = proplists:get_value(profile_id, Args),
			start = St,
			current = SF,
			spiral = SF,
			speed = Sp,
			spiralling = true,
			stamp = now()
			},
	case proplists:get_value(dontstart, Args, undefined) of
		undefined ->
			erlang:send_after(?DELTA, self(), move);
		true ->
			nop
	end,
	{ok, S}.

handle_call(_R, _F, _S) -> {stop, bad_call, undefined}.

handle_cast(move, S) -> move_me(S);

handle_cast(stop, S) ->
	{stop, normal, S}.

handle_info(move, S) -> 
	erlang:send_after(?DELTA, self(), move),
	move_me(S).

move_me(S) ->
	N = now(),
	Dt = timer:now_diff(N, S#details.stamp),
	Dd = Dt * S#details.speed,
	{R, W, A} = S#details.current,

	NPos = if
		% TODO should be relative to destination/start
		% which just happens to be origin
		R - ?TOLERANCE =< 0.0 ->
			lager:info("Spiral Robot is resetting."),
			S#details.spiral;

		S#details.spiralling ->
			%lager:debug("Now ~p : ~p", [Start, {R, W, A}]),
			Dw = Dd / R,
			%lager:debug("WHERE (~p) ~p / ~p = ~p", [A, Dd, R, Dw]),
			W2 = W + Dw,
			Cw = W2 - 2 * math:pi(),
			W3 = if
				Cw > 0 -> Cw;
				true -> W2
			end,
			{R - Dd, W3, A};

		true ->
			nop
	end,

	lager:debug("New Pos ~p -> ~p", [S#details.current, NPos]),
	%erlang:send_after(?DELTA, self(), move),
	{noreply, S#details{stamp = N, current = NPos}}.

terminate(_R, _S) -> ok.

code_change(_Old, S, _Extra) -> {ok, S}.
