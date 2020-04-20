-module(jpass_dns_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    io:format("jpass_dns_sup:start_link()~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    io:format("jpass_dns_sup:init()~n"),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => jpass_dns_server_listener,
		    start => {jpass_dns_server_listener, start_link, []},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_dns_server_listener]},
                  #{id => jpass_dns_sup_conns,
		    start => {jpass_dns_sup_conns, start_link, []},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => supervisor,
		    modules => [jpass_dns_sup_conns]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
