-module(jpass_dns_server_listener).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {server_socket=undefined, conn=undefined}).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("jpass_dns_server_listener:start_link()~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:format("jpass_dns_server_listener:init()~n"),
    {ok, #state{}, 1}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_server_listener:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    {reply, my_reply, State, 0}.
    
handle_cast(Request, State) ->
    io:format("jpass_dns_server_listener:handle_cast() request=~p, state=~p~n", [Request, State]),
    {noreply, State, 0}.

handle_info(Info, State) ->
    case Info of
        timeout -> 
            if State#state.server_socket == undefined ->
                ServerSocket = init_tcp_server(), 
               {noreply, State#state{server_socket = ServerSocket}, 0};
            %State#state.conn == undefined -> 
            true ->
                io:format("waiting for conn comming...~n"),
                {ok, Socket} = gen_tcp:accept(State#state.server_socket),
                io:format("a new conn connected.~n"),
                {ok, ChildPid} = jpass_dns_sup_conns:start_child(Socket),
                ok = gen_tcp:controlling_process(Socket, ChildPid),
                jpass_dns_server_conn:hand_off(ChildPid),
                {noreply, State, 0}
            end;
       {tcp_closed, _} ->
	   {noreply, State, 0};
        _ ->
           io:format("jpass_dns_server_listener:handle_info() info=~p, state=~p~n", [Info, State]),
	   {noreply, State, 0}
    end.

handle_continue(_Continue, State) ->
    io:format("jpass_dns_server_listener:handle_continue() continue=~p, state=~p~n", [_Continue, State]),
    {noreply, State, 0}.
    
code_change(_OldVsn, State, _Extra) -> 
    %io:format("jpass_dns_server_listener:code_change() oldVsn=~p, state=~p, extra=~p~n", [_OldVsn, _State, _Extra]),
    {ok, State, 0}.

format_status(_Opt, [_PDict, _State]) ->
    %io:format("jpass_dns_server_listener:format_status() opt=~p, pdict=~p, state=~p~n", [_Opt, _PDict, _State]),
    my_status_not_implemented.

terminate(Reason, _State) ->
    io:format("jpass_dns_server_listener:terminate() with reason: ~p~n", [Reason]),
    ok.

%% return server socket
init_tcp_server() ->
    Port = erlang:list_to_integer(os:getenv("JPASS_TCP_PORT", "53")),
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {ifaddr, loopback}, {reuseaddr, true}, {active, false}]),
    io:format("TCP server is listening on ~p~n", [Port]),
    ServerSocket.
