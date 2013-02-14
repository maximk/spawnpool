-module(spawner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(DOM0_HOST, "192.168.0.1").

-define(MAX_ZERGLINGS, 100).
-define(IPADDR_STARTS, {192,168,0,100}).

-define(GC_INTERVAL, 5000).

-record(st, {connection,running,pending,available}).

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

    {ok,Conn} = verx_client:start([{transport,verx_client_tcp},
								   {host,?DOM0_HOST}]),
    ok = verx:open(Conn, ["xen:///",0]),

	erlang:send_after(?GC_INTERVAL, self(), collect),
	{A,B,C,D} = ?IPADDR_STARTS,
	Available = [{A,B,C,D +N} || N <- lists:seq(0, ?MAX_ZERGLINGS -1)],
	{ok,#st{connection=Conn,running =[],pending=[],available =Available}}.

handle_call({create,_Vars}, _From, #st{available =[]} =St) ->
	{reply,{error,at_max_capacity},St};

handle_call({create,Vars}, From, #st{connection =Conn,
							  available =[Addr|Avail],
							  pending =Pend} =St) ->
	case do_create(Conn, Addr, Vars) of
	{ok,Name} ->
		{noreply,St#st{available =Avail,
					   pending =[{Name,Addr,From}|Pend]}};
	{error,Reason} ->
		{reply,{error,Reason},St}
	end.

handle_cast({ready,{A,B,C,D} =Addr}, #st{pending =Pend,running =Run} =St) ->
	io:format("zergling at ~w.~w.~w.~w reports on duty~n", [A,B,C,D]),
	{value,{Name,_,From},Pend1} = lists:keytake(Addr, 2, Pend),
	gen_server:reply(From, {ok,{Name,Addr}}),
    {noreply,St#st{pending =Pend1,running =[{Name,Addr}|Run]}}.

handle_info(collect, #st{connection =Conn} =St) ->
	erlang:send_after(?GC_INTERVAL, self(), collect),
    {noreply,do_collect(Conn, St)}.

terminate(_Reason, #st{connection =Conn} =_St) ->
	ok = verx:close(Conn),
	ok = verx_client:stop(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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

%%EOF
