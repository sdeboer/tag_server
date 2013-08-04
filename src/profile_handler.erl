-module(profile_handler).

-export([init/3]).

% Rest Standards
-export([
  rest_init/2,
  content_types_provided/2, content_types_accepted/2,
  allowed_methods/2
  ]).

% Callbacks
-export([
  to_json/2, create_profile/2
  ]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
  UUID = sessions:uuid(Req),
  Observer = profile:find_or_create_by_session(UUID),
  OID = profile:id(Observer),
  View = case cowboy_req:binding(profile_id, Req) of
    undefined ->
      Observer;
    ProfileId when OID == ProfileId->
      Observer;
    ProfileId ->
      profile:find(ProfileId)
  end,

  State = [{profile, View}, {observer, Observer}],

  {ok, Req, State}.

content_types_provided(Req, State) ->
  {[
      {{<<"application">>, <<"json">>, []}, to_json}
      ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>],
   Req, State}.

content_types_accepted(Req, State) ->
  {[
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, created_profile}],
   Req, State}.

to_json(Req, State) ->
  {"", Req, State}.


create_profile(Req, State) ->
  {"", Req, State}.
