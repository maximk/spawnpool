-module(spawnpool_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),
	application:start(spawnpool).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	Dispatch = cowboy_router:compile([
		{'_',[
			{'_',redirect_handler,[]}
		]}
	]),

	{ok,_} = cowboy:start_http(spawner, 100,
		[{port,8900},{ip,{127,0,0,1}}],
		[{env,[{dispatch,Dispatch}]}]),

	%% a listener to collect 'ready' messages from zerglings

	Notify = cowboy_router:compile([
		{'_',[
			{"/ready/[...]",notifier_handler,[]}
		]}
	]),

	{ok,_} = cowboy:start_http(notifier, 100,
		[{port,8909},{ip,{10,0,0,1}}],
		[{env,[{dispatch,Notify}]}]),

    spawnpool_sup:start_link().

stop(_State) ->
    ok.

%%EOF
