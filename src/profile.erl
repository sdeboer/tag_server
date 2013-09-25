-module(profile).

-export([
	find/1, find_by_session/1,
	find_or_create_by_session/1,
	save/1
	]).

-export([
	id/1,
	handle/1, handle/2,
	origin/2,
	coords/1, coords/2,
	to_json/1
	]).

-record(profile,
	{id = undefined,
		handle = <<"anonymous">>,
		session_id,
		origin = undefined,
		coords = undefined}).

-define(PREFIX, <<"profile">>).
-define(SESSION_AFFIX, <<"_session">>).

id(P) ->
	P#profile.id.

handle(P) ->
	P#profile.handle.

handle(P, New) ->
	P#profile{handle = New}.

origin(P, New) ->
	P#profile{origin = New}.

coords(P) ->
	P#profile.coords.

coords(P, New) ->
	P#profile{coords = New}.

find(PID) ->
	case persist:load(?PREFIX, PID) of
		undefined -> undefined;
		Data -> binary_to_term(Data)
	end.

save(P) ->
	ok = persist:save(?PREFIX, P),
	P.

find_by_session(SID) ->
	case persist:load([?PREFIX, ?SESSION_AFFIX], SID) of
		undefined -> undefined;
		PID -> find(PID)
	end.

find_or_create_by_session(SID) ->
	case find_by_session(SID) of
		undefined -> create(SID);
		P -> P
	end.

create(SID) ->
	lager:debug("Creating"),
	PID = list_to_binary(uuid:to_string(uuid:uuid4())),
	P = #profile{ id = PID },
	ok = persist:save(?PREFIX, PID, P),
	% FIXME This should have an expiry on it
	ok = persist:save([?PREFIX, ?SESSION_AFFIX], SID, PID),
	P.

to_json(P) ->
	Oid = P#profile.id,
	H = P#profile.handle,
	jiffy:encode({[
				{id, Oid},
				{handle, H}
				]}).
