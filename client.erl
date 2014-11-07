%%
%% An example Diameter client that can sends base protocol
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
%% packet and returns the answer.
%%

-module(client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("rfc4006_cc_Gy.hrl").
-include_lib("diameter_settings.hrl").

-export([start/1,     %% start a service
         connect/2,   %% add a connecting transport
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
         cast/0,
         charge_event/0,
         charge_event/2]).

-define(SVC_NAME, ?MODULE).
-define(APP_ALIAS, ?MODULE).
-define(CALLBACK_MOD, client_cb).
-define(DIAMETER_DICT_CCRA, rfc4006_cc_Gy).

-define(L, atom_to_list).

%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(Name), [{'Origin-Host', ?ORIGIN_HOST},
                        {'Origin-Realm', ?ORIGIN_REALM},
                        {'Vendor-Id', ?VENDOR_ID},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [?DCCA_APPLICATION_ID]},
                        {application, [{alias, ?APP_ALIAS},
                                       {dictionary, ?DIAMETER_DICT_CCRA},
                                       {module, ?CALLBACK_MOD}]}]).

%% start/1

start(Name)
    when is_atom(Name) ->
        diameter:start(),
        diameter:start_service(Name, ?SERVICE(Name)),
        connect({address, ?DIAMETER_PROTO, ?DIAMETER_IP, ?DIAMETER_PORT}).

start() ->
    start(?SVC_NAME).

%% connect/2

connect(Name, {address, Protocol, IPAddr, Port}) ->
    {ok, IP} = inet_parse:address(IPAddr),
    TransportOpts =  [{transport_module, tmod(Protocol)},
                      {transport_config, [
                        {reuseaddr, true},
                        {raddr, IP},
                        %{ip, {IP}},
                        {rport, Port}]}
                    ],
    diameter:add_transport(Name, {connect, [{reconnect_timer, 1000} | TransportOpts]}).


connect(Address) ->
    connect(?SVC_NAME, Address).

tmod(tcp)  -> diameter_tcp;
tmod(sctp) -> diameter_sctp.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

charge_event() ->
    charge_event(gprs, {"5511985231234", 1, 100, 1000000}).

charge_event(gprs, {MSISDN, ServiceID, RatingGroup, VolumeBytes}) ->
    SId = diameter:session_id(?L(?SVC_NAME)),
    ReqN = 0,
    % Generate initial CCR without MSCC
    Ret = create_session(gprs, {initial, MSISDN, SId, ReqN}),
    case Ret of
        {ok, _} ->
            io:format("CCR-INITIAL Success...~n"),
            rate_service(gprs, {update, MSISDN, SId, ReqN, {ServiceID, RatingGroup, 0, VolumeBytes}});
        {error, Err} ->
            io:format("Error: ~w~n", [Err])
    end,
    io:format("Event charged successfully.~n").

%% Create the PDP context. First CCR does not contain MSCC
create_session(gprs, {initial, MSISDN, SId, ReqN}) ->
    CCR = #rfc4006_cc_Gy_CCR{
        'Session-Id' = SId,
        'Auth-Application-Id' = 4,
        'Service-Context-Id' = "gprs@diameter.com",
        'CC-Request-Type' = ?CCR_INITIAL,
        'CC-Request-Number' = ReqN,
        'Event-Timestamp' = [calendar:now_to_local_time(now())],
        'Subscription-Id' = [#'rfc4006_cc_Gy_Subscription-Id' {
                                'Subscription-Id-Type' = ?'MSISDN',
                                'Subscription-Id-Data' = MSISDN
                            }],
        'Multiple-Services-Indicator' = [1]
    },
    diameter:call(?SVC_NAME, ?APP_ALIAS, CCR, []).

%% Rate service
rate_service(gprs, {update, MSISDN, SId, ReqN, {ServiceID, RatingGroup, ConsumedBytes, RemainingBytes}}) ->
    ReqN2 = ReqN+1,
    CCR1 = generate_MSCC(ServiceID, RatingGroup, ConsumedBytes, RemainingBytes),
    CCR2 = CCR1#rfc4006_cc_Gy_CCR{
            'Session-Id' = SId,
            'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
            'Service-Context-Id' = ?CONTEXT_ID,
            'CC-Request-Type' = ?CCR_UPDATE,
            'CC-Request-Number' = ReqN2,
            'Event-Timestamp' = [calendar:now_to_local_time(now())],
            'Subscription-Id' = [#'rfc4006_cc_Gy_Subscription-Id' {
                                    'Subscription-Id-Type' = ?'MSISDN',
                                    'Subscription-Id-Data' = MSISDN
                                }],
            'Called-Station-Id' = ["apn.com"],
            'Multiple-Services-Indicator' = [1]
            },
    Ret = diameter:call(?SVC_NAME, ?APP_ALIAS, CCR2, []),
    case Ret of
        {ok, CCA} ->
            io:format("CCR-UPDATE Success...~n"),
            %% Extract GSU from CCA
            #rfc4006_cc_Gy_CCA{
                  'Multiple-Services-Credit-Control' = MSCC
                } = CCA,
            [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
                    'Granted-Service-Unit' = GSU
                }|_] = MSCC,
            [#'rfc4006_cc_Gy_Granted-Service-Unit' {
                         'CC-Total-Octets' = [UsedUnits]
            }] = GSU,
            %% Subtract GSU from total
            NewRemainingBytes = RemainingBytes-UsedUnits,
            if
                (NewRemainingBytes > 0) ->
                    rate_service(gprs, {update, MSISDN, SId, ReqN2, {ServiceID, RatingGroup, UsedUnits, NewRemainingBytes}});
                (NewRemainingBytes =< 0) ->
                    io:format("Last request: ~w | ~w | ~w ~n", [UsedUnits, RemainingBytes, NewRemainingBytes]),
                    rate_service(gprs, {terminate, MSISDN, SId, ReqN2, {ServiceID, RatingGroup, RemainingBytes, 0}})
            end;
        {error, Err} ->
            io:format("Error: ~w~n", [Err])
    end,
    ok;

rate_service(gprs, {terminate, MSISDN, SId, ReqN, {ServiceID, RatingGroup, ConsumedBytes, RemainingBytes}}) ->
    ReqN2 = ReqN+1,
    CCR1 = generate_MSCC(ServiceID, RatingGroup, ConsumedBytes, RemainingBytes),
    CCR2 = CCR1#rfc4006_cc_Gy_CCR{
            'Session-Id' = SId,
            'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
            'Service-Context-Id' = ?CONTEXT_ID,
            'CC-Request-Type' = ?CCR_TERMINATE,
            'CC-Request-Number' = ReqN2,
            'Event-Timestamp' = [calendar:now_to_local_time(now())],
            'Subscription-Id' = [#'rfc4006_cc_Gy_Subscription-Id' {
                                    'Subscription-Id-Type' = ?'MSISDN',
                                    'Subscription-Id-Data' = MSISDN
                                }]
            'Called-Station-Id' = ["apn.com"],
            },
    Ret = diameter:call(?SVC_NAME, ?APP_ALIAS, CCR2, []),
    case Ret of
        {ok, _} ->
            io:format("CCR-TERMINATE Success...~n");
        {error, Err} ->
            io:format("Error: ~w~n", [Err])
    end,
    ok.


generate_MSCC(ServiceID, RatingGroup, ConsumedBytes, RemainingBytes) ->
    if
        ((ConsumedBytes == 0 ) and (RemainingBytes > 0)) ->
            %% First request. Must send RSU and no USU.
            MSCC = #rfc4006_cc_Gy_CCR {
            'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
                'Requested-Service-Unit' = [#'rfc4006_cc_Gy_Requested-Service-Unit' {
                     'CC-Total-Octets' = []
                 }],
                 'Service-Identifier' = [ServiceID],
                 'Rating-Group' = [RatingGroup]
            }]
            };
        ((ConsumedBytes /= 0 ) and (RemainingBytes > 0)) ->
            %% Update request. Must send RSU and USU.
            MSCC = #rfc4006_cc_Gy_CCR {
            'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
                'Requested-Service-Unit' = [#'rfc4006_cc_Gy_Requested-Service-Unit' {
                     'CC-Total-Octets' = []
                 }],
                'Used-Service-Unit' = [#'rfc4006_cc_Gy_Used-Service-Unit' {
                   'CC-Total-Octets' = [ConsumedBytes]
                }],
                 'Service-Identifier' = [ServiceID],
                 'Rating-Group' = [RatingGroup],
                 'Reporting-Reason' = [?'RFC4006_CC_GY_REPORTING-REASON_QUOTA_EXAUSTED']
            }]
            };
        ((ConsumedBytes /= 0 ) and (RemainingBytes =< 0) ) ->
            %% Last update request. Must send USU to report last used bytes. No RSU.
            MSCC = #rfc4006_cc_Gy_CCR {
                'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
                    'Used-Service-Unit' = [#'rfc4006_cc_Gy_Used-Service-Unit' {
                        'CC-Total-Octets' = [ConsumedBytes]
                    }],
                    'Service-Identifier' = [ServiceID],
                    'Rating-Group' = [RatingGroup],
                    'Reporting-Reason' = [?'RFC4006_CC_GY_REPORTING-REASON_FINAL']
                }]
            };
        true ->
            MSCC = err
    end,
    MSCC.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    diameter:stop_service(Name).

stop() ->
    stop(?SVC_NAME).

%% Internal Functions
