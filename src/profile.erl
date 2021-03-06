-module(profile).

-export([
	find/1, find_by_session/1, find_by_request/1,
	find_or_create_by_session/1,
	create/0, save/1
	]).

-export([
	id/1,
	handle/1, handle/2,
	origin/2,
	coords/1, coords/2,
	to_json/1,
	update/2
	]).

-record(profile,
	{id = undefined,
		handle = <<"anonymous">>,
		session_id,
		origin = undefined,
		coords = undefined}).

-define(PREFIX, ?MODULE_STRING).
-define(SESSION_AFFIX, <<"_session">>).

id(P) ->
	P#profile.id.

handle(P) ->
	P#profile.handle.

handle(New, P) ->
	P#profile{handle = New}.

origin(New, P) ->
	P#profile{origin = New}.

coords(P) ->
	P#profile.coords.

coords(New, P) ->
	P#profile{coords = New}.

find(PID) ->
	case persist:load([?PREFIX, PID]) of
		undefined -> undefined;
		Data -> binary_to_term(Data)
	end.

save(P) ->
	ok = persist:save([?PREFIX, id(P)], P),
	P.

update({List}, P) ->
	lager:debug("UPDAT ~p", [List]),
	case proplists:get_value(<<"handle">>, List) of
		undefined -> P;
		Handle when is_bitstring(Handle) ->
			P2 = handle(Handle, P),
			PID = id(P2),
			ok = persist:save([?PREFIX, PID], P2),
			P2;
		_Handle -> P
	end.

find_by_session(SID) ->
	case persist:load([?PREFIX, ?SESSION_AFFIX, SID]) of
		undefined -> undefined;
		PID -> {ok, find(PID)}
	end.

find_by_request(Req) ->
	case sessions:uuid(Req) of
		undefined -> undefined;
		SID -> find_by_session(SID)
	end.

find_or_create_by_session(SID) ->
	case find_by_session(SID) of
		{ok, P} -> {ok, P};
		undefined -> {ok, create(SID)}
	end.

create() ->
	PID = list_to_binary(uuid:to_string(uuid:uuid4())),
	P = #profile{ id = PID },
	ok = persist:save([?PREFIX, PID], P),
	P.

create(SID) ->
	lager:debug("Creating for ~p", [SID]),
	P = create(),
	PID = id(P),
	% FIXME This should have an expiry on it
	ok = persist:save([?PREFIX, ?SESSION_AFFIX, SID], PID),
	P.

to_json(P) ->
	Oid = P#profile.id,
	H = P#profile.handle,
	jiffy:encode({[
				{id, Oid},
				{handle, H}
				]}).
