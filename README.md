jpass_dns
=====

An OTP application

Build
-----
    $ rebar3 compile

Run
-----
    $ export JPASS_TCP_PORT=53 && export DNS_SERVER=8.8.8.8 && export DNS_PORT=53 && rebar3 shell

Release
-----
    $ rebar3 tar

Env settings
-----
    $ JPASS_TCP_PORT = 53
    $ DNS_SERVER = 8.8.4.4 
    $ DNS_PORT = 53 
