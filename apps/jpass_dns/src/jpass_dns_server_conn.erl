-module(jpass_dns_server_conn).

-behaviour(gen_server).

-export([start_link/1, hand_off/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {socket=undefined}).

-define(SERVER, ?MODULE).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    io:format("jpass_dns_server_conn:init() socket=~p~n", [Socket]),
    {ok, #state{socket=Socket}}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_server_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State}.
    
handle_cast(Request, State) ->
    Socket = State#state.socket,
    case Request of
        hand_off ->
            ok = inet:setopts(Socket, [{active, true}]);
        _ ->
	    io:format("jpass_dns_server_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
            ignore
    end,
    {noreply, State}.

handle_info(Info, State) ->
    Socket = State#state.socket,
    case Info of
        {tcp, Socket, DataBin} ->
            io:format("recvd: ~p~n", [DataBin]);
        {tcp_closed, _} ->
          ok = gen_tcp:close(State#state.socket),
          ok = gen_server:stop(erlang:self());
        _ ->
           io:format("jpass_dns_server_conn:handle_info() info=~p, state=~p~n", [Info, State])
    end,
   {noreply, State}.

handle_continue(_Continue, State) ->
    io:format("jpass_dns_server_conn:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_dns_server_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, State, _Extra]),
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_dns_server_conn:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_dns_server_conn:terminate() with reason: ~p~n", [Reason]),
    ok.

hand_off(ChildPid) ->
    gen_server:cast(ChildPid, hand_off).
