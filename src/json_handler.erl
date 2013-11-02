-module(json_handler).

-export([
	return_json/3,
	encode_response/4,
	construct_response/3, construct_response/4
	]).

return_json(Json, Req, S) ->
	Resp = case cowboy_req:qs_val(<<"jsonp">>, Req) of
		{undefined, _Req2} -> Json;
		{Fn, _Req2} ->
			[Fn, <<"(">>, Json, <<");">>]
	end,
	{Resp, Req, S}.

%encode_response(Resp, Req, State) -> construct_response(jiffy:encode(Resp), Req, State).

encode_response(Resp, Req, Code, State) ->
	construct_response(jiffy:encode(Resp), Req, Code, State).

construct_response(Json, Req, State) ->
	construct_response(Json, Req, State, 200).

construct_response(Json, Req, State, Code) ->
	{ok, R2} = cowboy_req:reply(
			Code,
			[{<<"content-type">>, <<"application/json">>}],
			Json,
			Req),
	{halt, R2, State}.
