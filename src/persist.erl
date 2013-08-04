-module(persist).
-behaviour(gen_server).

-define(DEFAULT_PREFIX, "tag").

% Model API
-export([save/2, save/3,
         load/1, load/2]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, handle_info/2, 
         code_change/3, stop/1]).

save(Key, Value) ->
  save(?DEFAULT_PREFIX, Key, Value).

save(Prefix, Key, Value) ->
  gen_server:call(?MODULE, {save_value, Prefix ++ ":" ++ Key, Value}).

load(Key) ->
  load(?DEFAULT_PREFIX, Key).

load(Prefix, Key) ->
  gen_server:call(?MODULE, {get_value, Prefix ++ ":" ++ Key}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Redis} = eredis:start_link(),
  {ok, Redis}.

stop(_Pid) ->
  stop().

stop() ->
  gen_server:cast(?MODULE, stop).

handle_call({get_value, Key}, _From, Redis) ->
  {ok, Value} = eredis:q(Redis, ["GET", Key]),
  {reply, Value, Redis};

handle_call({save_value, Key, Value}, _From, Redis) ->
  {ok, <<"OK">>} = eredis:q(Redis, ["SET", Key, Value]),
  {reply, ok, Redis};

handle_call(_Message, _From, Redis) ->
  {reply, error, Redis}.

handle_cast(_Message, Redis) -> {noreply, Redis}.
handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.
