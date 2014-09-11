-module(nominator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([fresh/0]).
-export([wait_until_ready/1]).

-record(st, {next =0,
			 taken =[],
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
	{ok,Domains} = egator:list(<<"zergling">>, []),
	Taken = integers([string:substr(binary_to_list(D), 9) || {D,_} <- Domains]),
    {ok,#st{taken =Taken}}.

handle_call(fresh, _From, #st{next =Next,taken =Taken} =St) ->
	Next1 = next_free(Next +1, Taken),
	DomName = list_to_binary(io_lib:format("zergling~w", [Next1])),
    {reply,DomName,St#st{next =Next1}};

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

integers(Ss) -> integers(Ss, []).

integers([], Acc) -> Acc;
integers([S|Ss], Acc) ->
	try I = list_to_integer(S),
		integers(Ss, [I|Acc])
	catch _:_ ->
		integers(Ss, Acc) end.

next_free(Next, Taken) ->
	case lists:member(Next, Taken) of
		true  -> next_free(Next +1, Taken);
		false -> Next end.

%%EOF
