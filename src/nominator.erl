-module(nominator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([fresh/0]).
-export([wait_until_ready/1]).

-record(st, {taken =[],
			 pending =[],
			 ready = []}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

fresh() ->
	gen_server:call(?SERVER, fresh).

wait_until_ready(DomName) ->
	gen_server:call(?SERVER, {wait_until_ready,DomName}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	random:seed(now()),
	{ok,Domains} = egator:list([]),
	Taken = domain_addresses([D || {D,_} <- Domains]),
    {ok,#st{taken =Taken}}.

handle_call(fresh, _From, #st{taken =Taken} =St) ->
	{ok,IpAddr,Taken1} = next_free(Taken),
	DomName = encode_name("zergling", IpAddr),
    {reply,{ok,DomName,IpAddr},St#st{taken =Taken1}};

handle_call({wait_until_ready,DomName}, From, #st{pending =Pending,ready =Ready} =St) ->
	case lists:keytake(DomName, 1, Ready) of
	{value,IpAddr,Ready1} ->
		{reply,IpAddr,St#st{ready =Ready1}};
	false ->
		{noreply,St#st{pending =[{DomName,From}|Pending]}}
	end.

handle_cast({ready,DomName,IpAddr}, #st{pending =Pending,ready =Ready} =St) ->
	case lists:keytake(DomName, 1, Pending) of
	{value,{_,From},Pending1} ->
		gen_server:reply(From, {ok,IpAddr}),
		{noreply,St#st{pending =Pending1}};
	false ->
		{noreply,St#st{ready =[{DomName,IpAddr}|Ready]}}
	end.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

domain_addresses(Domains) -> domain_addresses(Domains, []).

domain_addresses([], Acc) -> Acc;
domain_addresses([Dom|Domains], Acc) ->
	case re:run(Dom, "[a-z]_([0-9]+)_([0-9]+)_([0-9]+)",
						[{capture,all_but_first,list}]) of
	{match,[B,C,D]} ->
		IpAddr = {10,list_to_integer(B),list_to_integer(C),list_to_integer(D)},
		domain_addresses(Domains, [IpAddr|Acc]);
	_ ->
		domain_addresses(Domains, Acc)
	end.

next_free(Taken) ->
	B = random:uniform(256) -1,
	C = random:uniform(256) -1,
	D = random:uniform(256) -1,
	IpAddr = {10,B,C,D},
	case lists:member(IpAddr, Taken) of
		true  -> next_free(Taken);
		false -> {ok,IpAddr,[IpAddr|Taken]} end.

encode_name(Prefix, {10,B,C,D}) ->
	list_to_binary(io_lib:format("~s_~w_~w_~w", [Prefix,B,C,D])).

%%EOF
