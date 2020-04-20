-module(jpass_dns_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("jpass_dns_app:start()~n"),
    jpass_dns_sup:start_link().

stop(_State) ->
    io:format("jpass_dns_app:stop()~n"),
    ok.
