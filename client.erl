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
%% An example Diameter client that can sends base protocol RAR
%% requests to a connected peer.
%%
%% The simplest usage is as follows this to connect to a server
%% listening on the default port on the local host, assuming diameter
%% is already started (eg. diameter:start()).
%%
%%   client:start().
%%   client:connect(tcp).
%%   client:call().
%%
%% The first call starts the a service with the default name of
%% ?MODULE, the second defines a connecting transport that results in
%% a connection to the peer (if it's listening), the third sends it a
%% RAR and returns the answer.
%%

-module(client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("rfc4006_cc.hrl").

-export([start/1,     %% start a service
         connect/2,   %% add a connecting transport
         call/1,      %% send using the record encoding
         cast/1,      %% send using the list encoding and detached
         stop/1]).    %% stop a service
%% A real application would typically choose an encoding and whether
%% they want the call to return the answer or not. Sending with
%% both the record and list encoding here, one detached and one not,
%% is just for demonstration purposes.

%% Convenience functions using the default service name, ?SVC_NAME.
-export([start/0,
         connect/1,
         stop/0,
         call/0,
         cast/0]).

-define(SVC_NAME,     ?MODULE).
-define(APP_ALIAS,    ?MODULE).
-define(CALLBACK_MOD, client_cb).
-define(DIAMETER_DICT_CCRA, rfc4006_cc).

-define(L, atom_to_list).

%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(Name), [{'Origin-Host', ?L(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 0},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [4]},
                        {application, [{alias, ?APP_ALIAS},
                                       {dictionary, ?DIAMETER_DICT_CCRA},
                                       {module, ?CALLBACK_MOD}]}]).

%% start/1

start(Name)
    when is_atom(Name) ->
        peer:start(Name, ?SERVICE(Name)),
    connect(tcp).

start() ->
    start(?SVC_NAME).

%% connect/2

connect(Name, T) ->
    peer:connect(Name, T).

connect(T) ->
    connect(?SVC_NAME, T).

%% call/1

call(Name) ->
    SId = diameter:session_id(?L(Name)),
    CCR = #rfc4006_cc_CCR{
            'Session-Id' = SId,
            'Auth-Application-Id' = 4,
            'CC-Request-Type' = 1,
            'CC-Request-Number' = 0,
            'Service-Context-Id' = "diameter.com"
            },
        diameter:call(Name, ?APP_ALIAS, CCR, []).


call() ->
    call(?SVC_NAME).

%% cast/1

cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    CCR = ['CCR',
           {'Session-Id', SId},
            %'Session-Id' = diameter:session_id(?L(who)),
            {'Auth-Application-Id', 4},
            {'CC-Request-Type', 1},
            {'CC-Request-Number', 0},
            {'Service-Context-Id', "diameter.com"}
            ],
        diameter:call(Name, ?APP_ALIAS, CCR, [detach]).

cast() ->
    cast(?SVC_NAME).

%% stop/1

stop(Name) ->
    peer:stop(Name).

stop() ->
    stop(?SVC_NAME).
