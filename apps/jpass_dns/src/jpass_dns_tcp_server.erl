-module(jpass_dns_tcp_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2, code_change/3, format_status/2, terminate/2]).

-record(state, {}).

init([]) ->
    io:format("jpass_dns_tcp_server:init()~n"),
    {ok, #state{}, 100}.

handle_call(Request, From, State) ->
    io:format("jpass_dns_tcp_server:handle_call() request=~p, from=~p, state=~p~n", [Request, From, State]),
    NewState = State,
    {reply, my_reply, NewState}.
    
handle_cast(Request, State) ->
    io:format("jpass_dns_tcp_server:handle_cast() request=~p, state=~p~n", [Request, State]),
    NewState = State,
    {noreply, NewState}.

handle_info(Info, State) ->
    case Info of
        timeout -> 
            io:format("timeout~n"),
            tcp_listen;
        _ ->
           io:format("jpass_dns_tcp_server:handle_info() info=~p, state=~p~n", [Info, State])
    end,
    NewState = State,
    {noreply, NewState}.

handle_continue(Continue, State) ->
    io:format("jpass_dns_tcp_server:handle_continue() continue=~p, state=~p~n", [Continue, State]),
    NewState = State,
    {noreply, NewState}.
    
code_change(OldVsn, State, Extra) -> 
    io:format("jpass_dns_tcp_server:code_change() oldVsn=~p, state=~p, extra=~p~n", [OldVsn, State, Extra]),
    NewState = State,
    {ok, NewState}.

format_status(Opt, [PDict, State]) ->
    io:format("jpass_dns_tcp_server:format_status() opt=~p, pdict=~p, state=~p~n", [Opt, PDict, State]),
    my_status.

terminate(Reason, _State) ->
    io:format("jpass_dns_tcp_server:terminate() with reason: ~p~n", [Reason]),
    ok.
