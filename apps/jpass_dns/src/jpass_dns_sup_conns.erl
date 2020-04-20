-module(jpass_dns_sup_conns).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("jpass_dns_sup_conns:start_link()~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    io:format("jpass_dns_sup_conns:init()~n"),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => jpass_dns_server_conn,
		    start => {jpass_dns_server_conn, start_link, []},
		    restart => temporary,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_dns_server_conn]}],
    {ok, {SupFlags, ChildSpecs}}.

start_child(Socket) ->
    case supervisor:start_child(?SERVER, [Socket]) of
        {ok, _Child} -> ignore;
        {error, Reason} -> io:format("jpass_dns_sup_conns:start_child() error: ~p~n", [Reason])
    end,
    ok.
