-module(tag_server).

-export([start/0]).

start() ->
  lager:debug("Whip it good."),
  ok = application:start(crypto),
  ok = application:start(lager),
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(tag_server),
  lager:info("tag_server started"),
  ok.
