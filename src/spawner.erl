-module(spawner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(DEF_DOM0_HOST, "192.168.0.1").

-define(DEF_ADDR_BASE, {192,168,0,100}).
-define(DEF_NUM_ADDRS, 16).
-define(DEF_NUM_CONNS, 1).

-define(GC_INTERVAL, 5000).

-record(st, {avail_addrs,
			 avail_conns,
			 pending =[],
			 running =[],
			 gc_busy =false}).

-type id() :: binary().

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create/1,destroy/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Starts a new instance.
-spec create(_) -> {ok,id()} | {error,_}.

create(Vars) ->
	gen_server:call(?SERVER, {create,Vars}, infinity).

%% @doc Destroys an instance.
-spec destroy(id()) -> ok | {error,_}.

destroy(Id) ->
	gen_server:call(?SERVER, {destroy,Id}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->

	AddrBase = case init:get_argument('addr-base') of
	{ok,[[AddrStr]|_]} ->
		{ok,Addr} = inet_parse:address(AddrStr),
		Addr;
	error ->
		?DEF_ADDR_BASE
	end,

	NumAddrs = case init:get_argument('num-addrs') of
	{ok,[[NAStr]|_]} ->
		list_to_integer(NAStr);
	error ->
		?DEF_NUM_ADDRS
	end,

	NumConns = case init:get_argument('num-conns') of
	{ok,[[NCStr]|_]} ->
		list_to_integer(NCStr);
	error ->
		?DEF_NUM_CONNS
	end,

	Dom0Host = case init:get_argument('dom0-host') of
	{ok,[[Host]|_]} ->
		Host;
	error ->
		?DEF_DOM0_HOST
	end,

	AvailConns = [begin
					{ok,Pid} = libvirt_connection:start_link(Dom0Host),
					Pid
				  end || _ <- lists:seq(1, NumConns)],

	{A,B,C,D} = AddrBase,
	AvailAddrs = [{A,B,C,D +N}
					|| N <- lists:seq(0, NumAddrs -1)],

	schedule_gc(),

	{ok,#st{avail_conns =AvailConns,
			avail_addrs =AvailAddrs}}.

handle_call({create,_}, _From, #st{avail_conns =[]} =St) ->
	{reply,{error,connections_busy},St};

handle_call({create,_}, _From, #st{avail_addrs =[]} =St) ->
	{reply,{error,at_maximum_capacity},St};

handle_call({create,Vars}, From, #st{avail_conns =[Conn|AC],
									 avail_addrs =[Addr|AA],
									 pending =Pend} =St) ->
	libvirt_connection:begin_creation(Conn, Addr, Vars),
	{noreply,St#st{avail_conns =AC,
				   avail_addrs =AA,
				   pending =[{Addr,From}|Pend]}}.

handle_cast({ready,{A,B,C,D} =Addr}, #st{pending =Pend,running =Run} =St) ->
	io:format("zergling at ~w.~w.~w.~w reports on duty~n", [A,B,C,D]),
	case lists:keytake(Addr, 1, Pend) of
	{value,{_,From},Pend1} ->
		%% name is not known yet
		{noreply,St#st{pending =[{Addr,From,ready}|Pend1]}};
	{value,{_,From,Name},Pend1} ->
		gen_server:reply(From, {ok,{Name,Addr}}),
		{noreply,St#st{pending =Pend1,running =[{Name,Addr}|Run]}}
	end.

handle_info({created,Conn,Addr,{Name,_Uuid,_Rank}}, #st{avail_conns =AC,
														pending =Pend,
														running =Run} =St) ->
	case lists:keytake(Addr, 1, Pend) of
	{value,{_,From,ready},Pend1} ->
		%% ready message already received
		gen_server:reply(From, {ok,{Name,Addr}}),
		{noreply,St#st{avail_conns =AC ++ [Conn],
					   pending =Pend1,
					   running =[{Name,Addr}|Run]}};
	{value,{_,From},Pend1} ->
		{noreply,St#st{avail_conns =AC ++ [Conn],
					   pending =[{Addr,From,Name}|Pend1]}}
	end;

handle_info(collect, #st{gc_busy =true} =St) ->
	%% already under way - ignore
	schedule_gc(),
	{noreply,St};

handle_info(collect, #st{avail_conns =[]} =St) ->
	%% no connection to spare - ignore
	schedule_gc(),
	{noreply,St};

handle_info(collect, #st{avail_conns =[Conn|Avail]} =St) ->
	libvirt_connection:begin_collection(Conn),
	schedule_gc(),
    {noreply,St#st{avail_conns =Avail,gc_busy =true}};

handle_info({shutoff,{Name,_,_}}, #st{avail_addrs =AA,running =Run} =St) ->
	{value,{_,Addr},Run1} = lists:keytake(Name, 1, Run),
	{A,B,C,D} = Addr,
	io:format("~s shuts off, reuse ~w.~w.~w.~w~n", [Name,A,B,C,D]),
	{noreply,St#st{avail_addrs =[Addr|AA],running =Run1}};

handle_info({collected,Conn}, #st{avail_conns =AC} =St) ->
	{noreply,St#st{avail_conns =[Conn|AC],gc_busy =false}}.

terminate(_Reason, _St) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

<<<<<<< HEAD
do_create(Conn, Addr, OtherVars) ->
	{ok,ConfigXml} = build_config(Addr, OtherVars),

    {ok, [Domain]} = verx:domain_define_xml(Conn, [ConfigXml]),
    ok = verx:domain_create(Conn, [Domain]),
	{Name,_,_} = Domain,
    {ok,Name}.

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

do_collect(Conn, St0) ->
	{ok,[NumDef]} = verx:num_of_defined_domains(Conn),	
	{ok,[Shutoff]} = verx:list_defined_domains(Conn, [NumDef]),
	lists:foldl(fun(Id, #st{running =Run,available =Avail} =St) ->
		{ok,[{Name,_,_} =Domain]} = verx:domain_lookup_by_name(Conn, [Id]),
		case lists:keyfind(Name, 1, Run) of
		{_,{A,B,C,D} =Addr} ->
			io:format("gc: cleaning up '~s', ~w.~w.~w.~w available again~n",
								[Name,A,B,C,D]),
			verx:domain_undefine(Conn, [Domain]),
			St#st{available =[Addr|Avail]};
		_ ->
			St
		end
	end, St0, Shutoff).
=======
schedule_gc() ->
	erlang:send_after(?GC_INTERVAL, self(), collect).
>>>>>>> multi-conn

%%EOF
