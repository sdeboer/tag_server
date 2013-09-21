-module(tag_server).

-export([start/0]).

% Sync shouldn't really be necessary...Should be able to load the
% code myself darnit.
start() ->
	ok = application:start(syntax_tools),
	ok = application:start(lager),
	ok = application:start(crypto),
	persist:start_link(),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(tag_server),
	ok = application:start(sync),
	lager:info("tag_server started"),
	ok.
