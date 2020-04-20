-module(jpass_dns_server_conn).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {socket=undefined}).

-define(SERVER, ?MODULE).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    io:format("jpass_dns_server_conn:init() socket=~p~n", [Socket]),
    {ok, #state{}, 100}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_server_conn:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    NewState = State,
    {reply, my_reply, NewState}.
    
handle_cast(Request, State) ->
    io:format("jpass_dns_server_conn:handle_cast() request=~p, state=~p~n", [Request, State]),
    NewState = State,
    {noreply, NewState}.

handle_info(Info, State) ->
    case Info of
        timeout -> 
            if State#state.socket == undefined ->
                ServerSocket = init_tcp_server(), 
                {noreply, State#state{socket = ServerSocket}, 0};
            true -> 
                {ok, Socket} = gen_tcp:accept(State#state.socket),
                io:format("get socket: ~p~n", [Socket]),
                {noreply, State}
            end;
        _ ->
           io:format("jpass_dns_server_conn:handle_info() info=~p, state=~p~n", [Info, State]),
	   {noreply, State}
    end.

handle_continue(Continue, State) ->
    io:format("jpass_dns_server_conn:handle_continue() continue=~p, state=~p~n", [Continue, State]),
    NewState = State,
    {noreply, NewState}.
    
code_change(OldVsn, State, Extra) -> 
    io:format("jpass_dns_server_conn:code_change() oldVsn=~p, state=~p, extra=~p~n", [OldVsn, State, Extra]),
    NewState = State,
    {ok, NewState}.

format_status(Opt, [PDict, State]) ->
    io:format("jpass_dns_server_conn:format_status() opt=~p, pdict=~p, state=~p~n", [Opt, PDict, State]),
    my_status.

terminate(Reason, _State) ->
    io:format("jpass_dns_server_conn:terminate() with reason: ~p~n", [Reason]),
    ok.

%% return server socket
init_tcp_server() ->
    Port = erlang:list_to_integer(os:getenv("JPASS_TCP_PORT", "53")),
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, 2}, {active, true}]),
    io:format("TCP server is listening on ~p~n", [Port]),
    ServerSocket.
