-module(test).

-export([do/0, do_recv/1]).

do() ->
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(test, do_recv, [Sock]),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! go,
    ok = gen_tcp:close(LSock),
    do_finish.

do_recv(Sock) ->
    io:format("enter do_recv~n"),
    receive 
        go ->
            io:format("active: true~n"),
            ok = inet:setopts(Sock, [{active, true}]),
            do_recv(Sock);
        Any ->
            io:format("recevd: ~p~n", [Any]),
            do_recv(Sock)
    end.

