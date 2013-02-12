-module(notifier_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

init({tcp,http}, Req, []) ->
	{ok,Req,[]}.

handle(Req, St) ->
	{<<"POST">>,_} = cowboy_req:method(Req),
	{Addr,_} = cowboy_req:peer_addr(Req),
	gen_server:cast(spawner, {ready,Addr}),
	{ok,Req,St}.

terminate(_What, _Req, _St) ->
	ok.

%%EOF
