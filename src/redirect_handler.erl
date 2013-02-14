-module(redirect_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

init({tcp,http}, Req, []) ->
	{ok,Req,[]}.

handle(Req, St) ->
	TsReqReceived = timestamp(),

	{RealIP,_} = cowboy_req:header(<<"x-real-ip">>, Req, <<"UNKNOWN">>),
	io:format("~nnew request from ~s~n", [RealIP]),

	%% passed on to the new instance
	OtherVars = [{ts_req_received,TsReqReceived}],

	io:format("spawning a new instance...~n", []),
	case spawner:create(OtherVars) of
	{ok,{Name,Addr}} ->
		io:format("instance '~s' spawned~n", [Name]),

		{Path,_} = cowboy_req:path(Req),
		{A,B,C,D} = Addr,
		RedirectTo = io_lib:format("/internal_redirect/~w.~w.~w.~w/8000~s",
										[A,B,C,D,Path]),
		io:format("redirect to ~s~n", [RedirectTo]),

		Headers = [{<<"X-Accel-Redirect">>,list_to_binary(RedirectTo)}],
		{ok,Reply} = cowboy_req:reply(200, Headers, Req),
		{ok,Reply,St};

	{error,Error} ->
		{ok,Body} = error_page(Error),
		{ok,Reply} = cowboy_req:reply(200, [], Body, Req),
		{ok,Reply,St}
	end.

terminate(_What, _Req, _St) ->
	ok.

%%------------------------------------------------------------------------------

error_page(Error) ->
	Vars = [{error,io_lib:format("~p~n", [Error])},
			{max_inst,spawner:info(max_inst)}],
	error_dtl:render(Vars).

timestamp() ->
	{Mega,Secs,Micro} = now(),
	Mega *1000000.0 + Secs + Micro / 1000000.0.

%%EOF

