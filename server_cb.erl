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
%% The diameter application callback module configured by server.erl.
%%

-module(server_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("rfc4006_cc_Gy.hrl").

-define(CCR_INITIAL, ?'RFC4006_CC_GY_CC-REQUEST-TYPE_INITIAL_REQUEST').
-define(CCR_UPDATE, ?'RFC4006_CC_GY_CC-REQUEST-TYPE_UPDATE_REQUEST').
-define(CCR_TERMINATE, ?'RFC4006_CC_GY_CC-REQUEST-TYPE_TERMINATION_REQUEST').

-define(DIA_STATS_TAB, dcca_stats).

-define(MSISDN, ?'RFC4006_CC_GY_SUBSCRIPTION-ID-TYPE_END_USER_E164').
-define(IMSI, ?'RFC4006_CC_GY_SUBSCRIPTION-ID-TYPE_END_USER_IMSI').

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

peer_up(_SvcName, {PeerRef, _}, State) ->
    io:format("Peer up: ~p~n", [PeerRef]),
    State.

peer_down(_SvcName, {PeerRef, _}, State) ->
    io:format("Peer down: ~p~n", [PeerRef]),
    State.

pick_peer(_, _, _SvcName, _State) ->
    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

%% A request whose decode was successful ...
handle_request(#diameter_packet{msg = Req, errors = []}, _SvcName, {_, Caps})
  when is_record(Req, rfc4006_cc_Gy_CCR) ->
    io:format("CCR OK: ~p~n", [Req]),
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}
    } = Caps,
    #rfc4006_cc_Gy_CCR{
      'Session-Id' = SessionId,
      'Auth-Application-Id' = 4,
      'CC-Request-Type' = RT,
      'CC-Request-Number' = RN,
      %'Service-Context-Id' = ServiceContextId,
      'Event-Timestamp' = [EventTimestamp],
      'Subscription-Id' = [#'rfc4006_cc_Gy_Subscription-Id' {
          'Subscription-Id-Type' = ?'MSISDN',
          'Subscription-Id-Data' = MSISDN}],
      'Multiple-Services-Credit-Control' = MSCC
    } = Req,
    StartTime = timestamp(EventTimestamp),
    case MSCC of
      [_] ->
        MSCC_Data = process_mscc(RT, MSCC, {"APN.com", "7241234567890", MSISDN, "10.0.0.1", SessionId, StartTime});
      [] ->
        MSCC_Data = {}
    end,
    %io:format("CCR CC-Request-Type: ~p~n", [Req#rfc4006_cc_Gy_CCR.'CC-Request-Type']),
    %io:format("EventTimestamp ~p~n", [EventTimestamp]),
    {reply, answer(ok, RT, RN, SessionId, OH, OR, MSCC_Data)};


%% ... or one that wasn't. 3xxx errors are answered by diameter itself
%% but these are 5xxx errors for which we must contruct a reply.
%% diameter will set Result-Code and Failed-AVP's.
handle_request(#diameter_packet{msg = Req, errors = Err}, _SvcName, {_, Caps})
  when is_record(Req, rfc4006_cc_Gy_CCR) ->
    io:format("CCR Err: ~p~n", [Err]),
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #rfc4006_cc_Gy_CCR{'Session-Id' = Id,
                    'CC-Request-Type' = RT,
                    'CC-Request-Number'= RN}
        = Req,
    {reply, answer(err, RT, RN, Id, OH, OR, {})};

%% Should really reply to other base messages that we don't support
%% but simply discard them instead.
handle_request(#diameter_packet{}, _SvcName, {_,_}) ->
    io:format("Unsupported message"),
    discard.

%% Process MSCC events
%%
% process_mscc(?CCR_INITIAL, MSCC, {APN, IMSI, MSISDN, Location, SessionId, StartTime}) ->
%     common_stats:inc(?DIA_STATS_TAB, dia_input_initial_OK),
%     [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
%         'Used-Service-Unit' = [#'rfc4006_cc_Gy_Used-Service-Unit' {
%             'CC-Total-Octets' = USU_TotalOctets
%             %'CC-Input-Octets' = [USU_InputOctets],
%             %'CC-Output-Octets' = [USU_OutputOctets],
%             %'CC-Service-Specific-Units' = [USU_SpecificUnits]
%         }],
%         'Service-Identifier' = [ServiceID],
%         'Rating-Group' = [RatingGroup]
%         %'Final-Unit-Indication' = [],
%     }|_] = MSCC,

%     {ResultCode, GrantedUnits} = generate_intm_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, checkNullList(USU_TotalOctets), ServiceID, RatingGroup}),
%     {ServiceID, RatingGroup, GrantedUnits, ResultCode};

process_mscc(?CCR_UPDATE, MSCC, {APN, IMSI, MSISDN, Location, SessionId, StartTime}) ->
    common_stats:inc(?DIA_STATS_TAB, dia_input_update_OK),
    [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
    	'Used-Service-Unit' = USU,
    	'Requested-Service-Unit' = RSU,
        'Service-Identifier' = [ServiceID],
        'Rating-Group' = [RatingGroup]
    }|_] = MSCC,
    io:format("USU: ~w~n",[USU]),
    io:format("RSU: ~w~n",[RSU]),
    case {RSU, USU} of
    	% Have RSU. No USU (First interrogation)
    	{[_], []} ->
    		io:format("Have RSU. No USU (First interrogation)~n"),
    		{ResultCode, GrantedUnits} = generate_intm_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, 0, ServiceID, RatingGroup});
    	% Have RSU. Have USU (Next interrogation)
    	{[_], [_]} ->
    		io:format("Have RSU. Have USU (Next interrogation)~n"),
    		[#'rfc4006_cc_Gy_Used-Service-Unit' {
             'CC-Total-Octets' = [UsedUnits]
        	}] = USU,
			io:format("USU: ~w~n", [UsedUnits]),
    		{ResultCode, GrantedUnits} = generate_intm_req(update, {APN, IMSI, MSISDN, Location, SessionId, StartTime, UsedUnits, ServiceID, RatingGroup});
    	% No RSU. Have USU (Last interrogation)
    	{[], [_]} ->
    		io:format("No RSU. Have USU (Last interrogation)~n"),
    		[#'rfc4006_cc_Gy_Used-Service-Unit' {
             'CC-Total-Octets' = [UsedUnits]
        	}] = USU,
			io:format("USU: ~w~n",[UsedUnits]),
    		{ResultCode, GrantedUnits} = generate_intm_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, UsedUnits, ServiceID, RatingGroup})
    	end,
    {ServiceID, RatingGroup, GrantedUnits, ResultCode};

process_mscc(?CCR_TERMINATE, MSCC, {APN, IMSI, MSISDN, Location, SessionId, StartTime}) ->
    common_stats:inc(?DIA_STATS_TAB, dia_input_terminate_OK),
    [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
        'Used-Service-Unit' = [#'rfc4006_cc_Gy_Used-Service-Unit' {
            'CC-Total-Octets' = USU_TotalOctets
            %'CC-Input-Octets' = [USU_InputOctets],
            %'CC-Output-Octets' = [USU_OutputOctets],
            %'CC-Service-Specific-Units' = [USU_SpecificUnits]
        }],
        %'Tariff-Change-Usage' = [],
        'Service-Identifier' = [ServiceID],
        'Rating-Group' = [RatingGroup]
        %'G-S-U-Pool-Reference' = [],
        %'Validity-Time' = [],
        %'Result-Code' = [],
        %'Final-Unit-Indication' = [],
    }|_] = MSCC,

    {ResultCode, GrantedUnits} = generate_intm_req(terminate, {APN, IMSI, MSISDN, Location, SessionId, StartTime, checkNullList(USU_TotalOctets), ServiceID, RatingGroup}),
    {ServiceID, RatingGroup, GrantedUnits, ResultCode}.

%% Query OCS and get session quota
%%
generate_intm_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
    io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",50},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",0},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

  SimulatedGrantedQuota =  300000,
  ResultCode = 1,
  {ResultCode, SimulatedGrantedQuota};

generate_intm_req(update, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
    io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",3},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

  SimulatedGrantedQuota =  300000,
  ResultCode = 1,
  {ResultCode, SimulatedGrantedQuota};

generate_intm_req(terminate, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
	io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",2},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

  SimulatedGrantedQuota =  300000,
    ResultCode = 1,
  {ResultCode, SimulatedGrantedQuota}.

%% ---------------------------------------------------------------------------

%% Answer using the record or list encoding depending on
%% Re-Auth-Request-Type. This is just as an example. You would
%% typically just choose one, and this has nothing to do with the how
%% client.erl sends.

answer(ok, RT, RN, Id, OH, OR, {ServiceID, RatingGroup, GrantedUnits, ResultCode}) ->
  common_stats:inc(?DIA_STATS_TAB, event_OK),
  #rfc4006_cc_Gy_CCA {
    'Result-Code' = 2001, %% DIAMETER_SUCCESS
    'Origin-Host' = OH,
    'Origin-Realm' = OR,
    'Session-Id' = Id,
    'Auth-Application-Id' = 4,
    'CC-Request-Type' = RT,
    'CC-Request-Number' = RN,
    %'Termination-Cause' = [] %% Only used on TERMINATE
    'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Gy_Multiple-Services-Credit-Control' {
      'Granted-Service-Unit' = [#'rfc4006_cc_Gy_Granted-Service-Unit' {
        'CC-Total-Octets' = [GrantedUnits],
        'CC-Input-Octets' = [],
        'CC-Output-Octets' = [],
        'CC-Service-Specific-Units' = [],
        'AVP' = []
      }],
      'Service-Identifier' = [ServiceID],
      'Rating-Group' = [RatingGroup],
      'Validity-Time' = [3600],
      'Result-Code' = [2001]
      %'Final-Unit-Indication' = [],
    }]
  };

answer(ok, RT, RN, Id, OH, OR, {}) ->
  common_stats:inc(?DIA_STATS_TAB, event_OK),
  #rfc4006_cc_Gy_CCA {
    'Result-Code' = 2001, %% DIAMETER_SUCCESS
    'Origin-Host' = OH,
    'Origin-Realm' = OR,
    'Session-Id' = Id,
    'Auth-Application-Id' = 4,
    'CC-Request-Type' = RT,
    'CC-Request-Number' = RN
  };

answer(err, RT, RN, Id, OH, OR, {}) ->
    common_stats:inc(?DIA_STATS_TAB, event_ERR),
    #rfc4006_cc_Gy_CCA {
      'Result-Code' = 5012, %% DIAMETER_UNABLE_TO_COMPLY
      'Origin-Host' = OH,
      'Origin-Realm' = OR,
      'Session-Id' = Id,
      'Auth-Application-Id' = 4,
      'CC-Request-Type' = RT,
      'CC-Request-Number' = RN
    }.

%% Internal Functions

timestamp({{YY, MM, DD}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [YY, MM, DD, Hour, Min, Sec]).

% Check list. If null, return zero, else return value.
checkNullList([X]) -> X;
checkNullList([]) -> 0.