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
-include_lib("rfc4006_cc.hrl").

-define(CCR_INITIAL, ?'RFC4006_CC_CC-REQUEST-TYPE_INITIAL_REQUEST').
-define(CCR_UPDATE, ?'RFC4006_CC_CC-REQUEST-TYPE_UPDATE_REQUEST').
-define(CCR_TERMINATE, ?'RFC4006_CC_CC-REQUEST-TYPE_TERMINATION_REQUEST').

-define(DIA_STATS_TAB, dcca_stats).

-define(MSISDN, ?'RFC4006_CC_SUBSCRIPTION-ID-TYPE_END_USER_E164').
-define(IMSI, ?'RFC4006_CC_SUBSCRIPTION-ID-TYPE_END_USER_IMSI').

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
    io:format("up: ~p~n", [PeerRef]),
    State.

peer_down(_SvcName, {PeerRef, _}, State) ->
    io:format("down: ~p~n", [PeerRef]),
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
  when is_record(Req, rfc4006_cc_CCR) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}
    } = Caps,
    io:format("CCR OK: ~p~n", [Req]),
    #rfc4006_cc_CCR{
      'Session-Id' = Id,
      'Auth-Application-Id' = 4,
      'CC-Request-Type' = RT,
      'CC-Request-Number' = RN,
      'Service-Context-Id' = ServiceContextId,
      'Subscription-Id' = [#'rfc4006_cc_Subscription-Id' {
          'Subscription-Id-Type' = ?'MSISDN',
          'Subscription-Id-Data' = MSISDN}]
    } = Req,

    %io:format("CCR CC-Request-Type: ~p~n", [Req#rfc4006_cc_CCR.'CC-Request-Type']),
    case Req#rfc4006_cc_CCR.'CC-Request-Type' of
      CCR_INITIAL ->
        common_stats:inc(?DIA_STATS_TAB, dia_input_initial_OK),
        #rfc4006_cc_CCR {
          %'Termination-Cause' = [] %% Only used on TERMINATE
          %'Multiple-Services-Indicator' = [_],
          'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Multiple-Services-Credit-Control' {
            %'Requested-Service-Unit' = [#'rfc4006_cc_Requested-Service-Unit' {
            %    'CC-Total-Octets' = [],
            %    'CC-Input-Octets' = [],
            %    'CC-Output-Octets' = [],
            %    'CC-Service-Specific-Units' = [],
            %    'AVP' = []
            %}],
            %'Used-Service-Unit' = [#'rfc4006_cc_Used-Service-Unit' {
            %    'CC-Total-Octets' = [USU_TotalOctets],
            %    'CC-Input-Octets' = [USU_InputOctets],
            %    'CC-Output-Octets' = [USU_OutputOctets],
            %    'CC-Service-Specific-Units' = [USU_SpecificUnits]
            %}],
            %'Tariff-Change-Usage' = [],
            'Service-Identifier' = [ServiceID],
            'Rating-Group' = [RatingGroup]
            %'G-S-U-Pool-Reference' = [],
            %'Validity-Time' = [],
            %'Result-Code' = [],
            %'Final-Unit-Indication' = [],

        }|_]
      } = Req
      %CCR_UPDATE ->
      %common_stats:inc(?DIA_STATS_TAB, dia_input_update_OK),
      %CCR_TERMINATE ->
      %common_stats:inc(?DIA_STATS_TAB, dia_input_terminate_OK),
    end,
    {reply, answer(ok, RT, RN, Id, OH, OR, ServiceID, RatingGroup)};

%% ... or one that wasn't. 3xxx errors are answered by diameter itself
%% but these are 5xxx errors for which we must contruct a reply.
%% diameter will set Result-Code and Failed-AVP's.
handle_request(#diameter_packet{msg = Req, errors = Err}, _SvcName, {_, Caps})
  when is_record(Req, rfc4006_cc_CCR) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #rfc4006_cc_CCR{'Session-Id' = Id,
                    'CC-Request-Type' = RT,
                    'CC-Request-Number'= RN}
        = Req,
    io:format("CCR Err: ~p~n", [Err]),
    {reply, answer_err(RT, RN, Id, OH, OR)};

%% Should really reply to other base messages that we don't support
%% but simply discard them instead.
handle_request(#diameter_packet{}, _SvcName, {_,_}) ->
    discard.

%% ---------------------------------------------------------------------------

%% Answer using the record or list encoding depending on
%% Re-Auth-Request-Type. This is just as an example. You would
%% typically just choose one, and this has nothing to do with the how
%% client.erl sends.

answer(ok, RT, RN, Id, OH, OR, ServiceID, RatingGroup) ->
  common_stats:inc(?DIA_STATS_TAB, event_OK),
  #rfc4006_cc_CCA{
    'Result-Code' = 2001, %% DIAMETER_SUCCESS
    'Origin-Host' = OH,
    'Origin-Realm' = OR,
    'Session-Id' = Id,
    'Auth-Application-Id' = 4,
    'CC-Request-Type' = RT,
    'CC-Request-Number' = RN,
    %'Termination-Cause' = [] %% Only used on TERMINATE
    %'Subscription-Id' = [#'rfc4006_cc_Subscription-Id' {
    %                        'Subscription-Id-Type' = ?'MSISDN',
    %                        'Subscription-Id-Data' = MSISDN
    %                    }],
    'Multiple-Services-Credit-Control' = [#'rfc4006_cc_Multiple-Services-Credit-Control' {
      'Granted-Service-Unit' = [#'rfc4006_cc_Granted-Service-Unit' {
        'CC-Total-Octets' = [300000],
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
  }.
answer_err(RT, RN, Id, OH, OR) ->
    common_stats:inc(?DIA_STATS_TAB, event_ERR),
    #rfc4006_cc_CCA{'Result-Code' = 5012, %% DIAMETER_UNABLE_TO_COMPLY
                    'Origin-Host' = OH,
                    'Origin-Realm' = OR,
                    'Session-Id' = Id,
                    'Auth-Application-Id' = 4,
                    'CC-Request-Type' = RT,
                    'CC-Request-Number' = RN}.