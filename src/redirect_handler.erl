-module(redirect_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

-define(ZERGLING_IMAGE, <<"/home/mk/zergling/vmling">>).
-define(BRIDGE, <<"xenbr0">>).
-define(EXTRA, "-dhcp -notify 10.0.0.1:8909 -shutdown_after 15000 -home /zergling "
			   "-pz /zergling/ebin "
			   "-pz /zergling/deps/cowboy/ebin "
			   "-pz /zergling/deps/cowlib/ebin "
			   "-pz /zergling/deps/ranch/ebin "
			   "-pz /zergling/deps/erlydtl/ebin "
			   "-s zergling_app").

init({tcp,http}, Req, []) ->
	{ok,Req,[]}.

handle(Req, St) ->
	TsReqReceived = timestamp(),
	DomName = nominator:fresh(),
	{RealIp,_} = cowboy_req:header(<<"x-real-ip">>, Req, undefined),
	io:format("demo:~s:~s:", [real_host_name(RealIp),DomName]),

	Extra = ?EXTRA ++ io_lib:format(" -ts_req_received ~w", [TsReqReceived]),
	DomOpts = [{extra,list_to_binary(Extra)},
			   {memory,32},
			   {bridge,?BRIDGE}],
	case egator:create(DomName, ?ZERGLING_IMAGE, DomOpts, []) of
	ok ->
		{ok,{A,B,C,D}} = nominator:wait_until_ready(DomName),
		io:format("~w.~w.~w.~w\n", [A,B,C,D]),
		{Path,_} = cowboy_req:path(Req),
		RedirectTo = io_lib:format("/internal_redirect/~w.~w.~w.~w/8000~s",
										[A,B,C,D,Path]),
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

real_host_name(undefined) -> <<"not-set">>;
real_host_name(IpAddr) when is_binary(IpAddr) ->
	case inet:gethostbyaddr(binary_to_list(IpAddr)) of
		{error,_}						 -> IpAddr;
		{ok,{hostent,undefined,_,_,_,_}} -> IpAddr;
		{ok,{hostent,HostName,_,_,_,_}}  -> list_to_binary(HostName) end.

error_page(Error) ->
	Vars = [{error,io_lib:format("~p~n", [Error])}],
	error_dtl:render(Vars).

timestamp() ->
	{Mega,Secs,Micro} = now(),
	Mega *1000000.0 + Secs + Micro / 1000000.0.

%%EOF
