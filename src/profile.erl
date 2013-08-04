-module(profile).

-export([
  find/1, find_by_session/1,
  find_or_create_by_session/1,
  id/1
  ]).

-record(profile,
        {id = undefined,
         label = <<"anonymous">>,
         session_id,
         origin = undefined,
         coords = undefined}).

-define(PREFIX, "profile").

id(P) ->
  P#profile.id.

find(ProfileID) ->
  P = persist:load(?PREFIX, ProfileID),
  P.

find_by_session(SessionId) ->
  find(SessionId),
  bad.

find_or_create_by_session(SessionId) ->
  P = #profile{label = <<"Someone">>, session_id = SessionId},
  P.
