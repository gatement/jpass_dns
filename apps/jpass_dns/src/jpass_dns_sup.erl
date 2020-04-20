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
    ChildSpecs = [#{id => jpass_dns_tcp_server,
		    start => {gen_server, start_link, [jpass_dns_tcp_server, [], []]},
		    restart => permanent,
		    shutdown => brutal_kill,
		    type => worker,
		    modules => [jpass_dns_tcp_server]}],
    {ok, {SupFlags, ChildSpecs}}.
