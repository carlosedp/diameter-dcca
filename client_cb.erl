%%
%% The diameter application callback module configured by client.erl.
%%
-module(client_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("rfc4006_cc_Gy.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(#diameter_packet{msg = ['CCR' = T | Avps]}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}
    } = Caps,
    {send, [T, {'Origin-Host', OH},
               {'Origin-Realm', OR},
               {'Destination-Host', [DH]},
               {'Destination-Realm', DR}
             | Avps]};

prepare_request(#diameter_packet{msg = Rec}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}
    } = Caps,

    {send, Rec#rfc4006_cc_Gy_CCR{'Origin-Host' = OH,
                              'Origin-Realm' = OR,
                              'Destination-Host' = [DH],
                              'Destination-Realm' = DR}}.

%% prepare_retransmit/3

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

%% handle_answer/4

%% Since client.erl has detached the call when using the list
%% encoding and not otherwise, output to the terminal in the
%% the former case, return in the latter.

handle_answer(#diameter_packet{msg = Msg}, Request, _SvcName, _Peer)
    when is_list(Request) ->
        io:format("CCA: ~p~n", [Msg]),
        {ok, Msg};

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer) ->
    {ok, Msg}.

%% handle_error/4

handle_error(Reason, Request, _SvcName, _Peer)
    when is_list(Request) ->
        %io:format("error: ~p~n", [Reason])
        {error, Reason};

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).
