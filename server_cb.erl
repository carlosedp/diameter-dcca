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
-include_lib("diameter_settings.hrl").

-define(DIA_STATS_TAB, dcca_stats).

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
      'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
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
      	%% TODO -> Receive fields from request. Validate all fields.
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
    		{ResultCode, GrantedUnits} = ocs_intm:generate_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, 0, ServiceID, RatingGroup});
    	% Have RSU. Have USU (Next interrogation)
    	{[_], [_]} ->
    		io:format("Have RSU. Have USU (Next interrogation)~n"),
    		[#'rfc4006_cc_Gy_Used-Service-Unit' {
             'CC-Total-Octets' = [UsedUnits]
        	}] = USU,
			io:format("USU: ~w~n", [UsedUnits]),
    		{ResultCode, GrantedUnits} = ocs_intm:generate_req(update, {APN, IMSI, MSISDN, Location, SessionId, StartTime, UsedUnits, ServiceID, RatingGroup});
    	% No RSU. Have USU (Last interrogation)
    	{[], [_]} ->
    		io:format("No RSU. Have USU (Last interrogation)~n"),
    		[#'rfc4006_cc_Gy_Used-Service-Unit' {
             'CC-Total-Octets' = [UsedUnits]
        	}] = USU,
			io:format("USU: ~w~n",[UsedUnits]),
    		{ResultCode, GrantedUnits} = ocs_intm:generate_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, UsedUnits, ServiceID, RatingGroup})
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

    {ResultCode, GrantedUnits} = ocs_intm:generate_req(terminate, {APN, IMSI, MSISDN, Location, SessionId, StartTime, checkNullList(USU_TotalOctets), ServiceID, RatingGroup}),
    {ServiceID, RatingGroup, GrantedUnits, ResultCode}.



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
    'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
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
    'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
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
      'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
      'CC-Request-Type' = RT,
      'CC-Request-Number' = RN
    }.

%% Internal Functions

timestamp({{YY, MM, DD}, {Hour, Min, Sec}}) ->
    io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [YY, MM, DD, Hour, Min, Sec]).

% Check list. If null, return zero, else return value.
checkNullList([X]) -> X;
checkNullList([]) -> 0.
