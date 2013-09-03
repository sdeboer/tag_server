-module(tag_server).

-export([start/0]).

% Sync shouldn't really be necessary...
start() ->
	ok = application:start(syntax_tools),
	ok = application:start(lager),
	ok = application:start(crypto),
	persist:start_link(),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(sync),
	ok = application:start(tag_server),
	lager:info("tag_server started"),
	lager:debug("BUgger"),
	ok.
