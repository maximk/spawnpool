-module(libvirt_connection).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([begin_creation/3]).
-export([begin_collection/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Dom0Host) ->
    gen_server:start_link(?MODULE, [Dom0Host], []).

begin_creation(Conn, Addr, Conf) ->
	gen_server:cast(Conn, {begin_creation,self(),Addr,Conf}).

begin_collection(Conn) ->
	gen_server:cast(Conn, {begin_collection,self()}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Dom0Host) ->

    {ok,Ref} = verx_client:start([{transport,verx_client_tcp},
								  {host,Dom0Host}]),
    ok = verx:open(Ref, ["xen:///",0]),
	
	{ok,Ref}.

handle_call(_Msg, _From, St) ->
   {reply,ok,St}.

handle_cast({begin_creation,ReplyTo,Addr,Conf}, Ref =St) ->
	{ok,ConfigXml} = build_config(Addr, Conf),
    {ok, [Domain]} = verx:domain_define_xml(Ref, [ConfigXml]),
    ok = verx:domain_create(Ref, [Domain]),
	ReplyTo ! {created,self(),Addr,Domain},
	{noreply,St};

handle_cast({begin_collection,ReplyTo}, Ref =St) ->
	{ok,[NumDef]} = verx:num_of_defined_domains(Ref),	
	{ok,[Shutoff]} = verx:list_defined_domains(Ref, [NumDef]),
	lists:foreach(fun(Id) ->
		{ok,[Domain]} = verx:domain_lookup_by_name(Ref, [Id]),
		%X = verx:domain_get_cpu_stats(Ref, [Name]),
		%io:format("CPU stats = ~p~n",[X]),
		io:format("gc: undefine domian ~p~n", [Domain]),
		verx:domain_undefine(Ref, [Domain]),
		ReplyTo ! {shutoff,Domain}
	end, Shutoff),
	ReplyTo ! {collected,self()},
	{noreply,St}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, Ref) ->
	ok = verx:close(Ref),
	ok = verx_client:stop(Ref).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_config(Addr, OtherVars) ->
	build_config(Addr, OtherVars, 32).

build_config({A,B,C,D}, OtherVars, Memory) ->
	IpAddr = io_lib:format("~w.~w.~w.~w", [A,B,C,D]),
	Name = "zergling" ++ integer_to_list(D),
	Vars = [{inst,[
				{name,Name},
				{memory,Memory},
				{ipaddr,IpAddr}
			]}]
				++ OtherVars,
	zergling_config_dtl:render(Vars).

%%EOF
