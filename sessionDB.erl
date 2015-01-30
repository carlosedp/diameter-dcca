-module(sessionDB).

-define(SESSION_TAB, session_tab).

-define(CMD_SUCESS, 0).
-define(CMD_ERROR, 65).
-define(CMD_NOSUBSCRIBER, 66).

-define(SVC_SUCESS, 0).
-define(SVC_FREE, 2).
-define(SVC_OUTOFMONEY, 64).
-define(SVC_DENIED, 65).
-define(SVC_DUPLICATE, 70).
-define(SVC_ERROR, 69).

-record(onlineSession,
    {sessionId, msisdn, state, timeout, consumedAmount, allocatedAmount,
    consumedCost, allocatedCost, callType, callStart, ticketType,
    otherParty, homeZone, otherZone, tariffName}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([create_session_tab/0,
         stop_session_tab/0,
         clear_session_tab/0,
         get_resp/3]).

create_session_tab() ->
    ets:new(?SESSION_TAB, [set, public, named_table]).
stop_session_tab() ->
    ets:delete(?SESSION_TAB).
clear_session_tab() ->
    ets:delete_all_objects(?SESSION_TAB).

get_resp(SID, RT, RN) ->
    get_resp(SID, RT, RN, ?SESSION_TAB).
get_resp(SID, RT, RN, Session_Tid) ->
    case RT of
        1 -> event;
        2 -> case check_sesion(Session_Tid, SID, RN) of
                 new_sid -> ets:insert(Session_Tid, {SID,{RT, RN}}),
                            start;
                 _Others -> duplicate_start
             end;
        3 -> case check_sesion(Session_Tid, SID, RN) of
                 new_sid -> unknow_sid;
                 new_msg -> ets:insert(Session_Tid, {SID,{RT, RN}}),
                            interim;
                 old_msg -> expired_interim
             end;
        4 -> case check_sesion(Session_Tid, SID, RN) of
                 new_sid -> unknow_sid;
                 new_msg -> ets:delete(Session_Tid, SID),
                            stop;
                 old_msg -> invalid_stop
             end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
check_sesion(Session_Tid, SID, RN) ->
%%  T1 = erlang:now(),
    Res = case ets:lookup(Session_Tid, SID) of
              [] -> new_sid;
              [{SID, {_OldRT, OldRN}}|_] ->
                  if
                      RN > OldRN -> new_msg;
                      true -> old_msg
                  end
          end,
%%  io:format("spend ~p ms ~n", [timer:now_diff(erlang:now(), T1)/1000]),
    Res.
