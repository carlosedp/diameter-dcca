%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% An example Diameter server that can respond to the base protocol
%% RAR sent by the client example.
%%
%% The simplest example to start a server listening on the loopback
%% address (which will serve the example usage given in client.erl) is
%% like this assuming diameter is already started (eg. diameter:start()):
%%
%%   server:start().
%%   server:listen(tcp).
%%
%% The first call starts a service, the second adds a transport listening
%% on the default port.
%%

-module(server).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("diameter_settings.hrl").

-export([start/1,    %% start a service
         listen/2,   %% add a listening transport
         stop/1]).   %% stop a service

%% Convenience functions using the default service name, ?SVC_NAME.
-export([start/0,
         listen/1,
         stop/0]).

-define(DIA_STATS_TAB, dcca_stats).
-define(DIA_STATS_COUNTERS, [event_OK, event_ERR]).

%% Server parameters
-define(SVC_NAME,     ?MODULE).
-define(APP_ALIAS,    ?MODULE).
-define(CALLBACK_MOD, server_cb).
-define(DIAMETER_DICT_CCRA, rfc4006_cc_Gy).

%% The service configuration. In a server supporting multiple Diameter
%% applications each application may have its own, although they could all
%% be configured with a common callback module.
-define(SERVICE(Name), [{'Origin-Host', ?ORIGIN_HOST},
                        {'Origin-Realm', ?ORIGIN_REALM},
                        {'Vendor-Id', ?VENDOR_ID},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [?DCCA_APPLICATION_ID]},
                        {application,
                            [{alias, ?APP_ALIAS},
                            {dictionary, ?DIAMETER_DICT_CCRA},
                            {module, ?CALLBACK_MOD}]
                        }]).


%% start/1

start(Name)
    when is_atom(Name) ->
        diameter:start(),
        common_stats:init(?DIA_STATS_TAB, ?DIA_STATS_COUNTERS),
        diameter:start_service(Name, ?SERVICE(Name)),
        listen({address, ?DIAMETER_PROTO, ?DIAMETER_IP, ?DIAMETER_PORT}).

start() ->
    start(?SVC_NAME).


%% listen/2

listen(Name, {address, Protocol, IPAddr, Port}) ->
    {ok, IP} = inet_parse:address(IPAddr),
    TransportOpts =  [{transport_module, tmod(Protocol)},
                      {transport_config, [{reuseaddr, true},
                      {ip, IP}, {port, Port}]}],
    diameter:add_transport(Name, {listen, TransportOpts}).

listen(Address) ->
    listen(?SVC_NAME, Address).

%% stop/1

stop(Name) ->
    common_stats:terminate(?DIA_STATS_TAB),
    diameter:stop_service(Name).

stop() ->
    stop(?SVC_NAME).

tmod(tcp)  -> diameter_tcp;
tmod(sctp) -> diameter_sctp.

