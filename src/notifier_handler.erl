-module(notifier_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

init({tcp,http}, Req, []) ->
	{ok,Req,[]}.

handle(Req, St) ->
	{<<"POST">>,_} = cowboy_req:method(Req),
	{[DomName],_} = cowboy_req:path_info(Req),
	{{IpAddr,_Port},_} = cowboy_req:peer(Req),
	gen_server:cast(nominator, {ready,DomName,IpAddr}),
	{ok,Req,St}.

terminate(_What, _Req, _St) ->
	ok.

%%EOF
