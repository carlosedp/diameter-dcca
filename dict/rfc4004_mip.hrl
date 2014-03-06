%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

%%
%% Copyright (c) Ericsson AB. All rights reserved.
%%
%% The information in this document is the property of Ericsson.
%%
%% Except as specifically authorized in writing by Ericsson, the
%% receiver of this document shall keep the information contained
%% herein confidential and shall protect the same in whole or in
%% part from disclosure and dissemination to third parties.
%%
%% Disclosure and disseminations to the receivers employees shall
%% only be made on a strict need to know basis.
%%

-hrl_name('rfc4004_mip.hrl').


%%% -------------------------------------------------------
%%% Message records:
%%% -------------------------------------------------------

-record(rfc4004_mip_AMR,
	{'Session-Id', 'Auth-Application-Id', 'User-Name',
	 'Destination-Realm', 'Origin-Host', 'Origin-Realm',
	 'MIP-Reg-Request', 'MIP-MN-AAA-Auth',
	 'Acct-Multi-Session-Id' = [], 'Destination-Host' = [],
	 'Origin-State-Id' = [], 'MIP-Mobile-Node-Address' = [],
	 'MIP-Home-Agent-Address' = [],
	 'MIP-Feature-Vector' = [],
	 'MIP-Originating-Foreign-AAA' = [],
	 'Authorization-Lifetime' = [],
	 'Auth-Session-State' = [], 'MIP-FA-Challenge' = [],
	 'MIP-Candidate-Home-Agent-Host' = [],
	 'MIP-Home-Agent-Host' = [], 'MIP-HA-to-FA-SPI' = [],
	 'Proxy-Info' = [], 'Route-Record' = [], 'AVP' = []}).

-record(rfc4004_mip_AMA,
	{'Session-Id', 'Auth-Application-Id', 'Result-Code',
	 'Origin-Host', 'Origin-Realm',
	 'Acct-Multi-Session-Id' = [], 'User-Name' = [],
	 'Authorization-Lifetime' = [],
	 'Auth-Session-State' = [], 'Error-Message' = [],
	 'Error-Reporting-Host' = [],
	 'Re-Auth-Request-Type' = [], 'MIP-Feature-Vector' = [],
	 'MIP-Reg-Reply' = [], 'MIP-MN-to-FA-MSA' = [],
	 'MIP-MN-to-HA-MSA' = [], 'MIP-FA-to-MN-MSA' = [],
	 'MIP-FA-to-HA-MSA' = [], 'MIP-HA-to-MN-MSA' = [],
	 'MIP-MSA-Lifetime' = [], 'MIP-Home-Agent-Address' = [],
	 'MIP-Mobile-Node-Address' = [], 'MIP-Filter-Rule' = [],
	 'Origin-State-Id' = [], 'Proxy-Info' = [], 'AVP' = []}).

-record(rfc4004_mip_HAR,
	{'Session-Id', 'Auth-Application-Id',
	 'Authorization-Lifetime', 'Auth-Session-State',
	 'MIP-Reg-Request', 'Origin-Host', 'Origin-Realm',
	 'User-Name', 'Destination-Realm', 'MIP-Feature-Vector',
	 'Destination-Host' = [], 'MIP-MN-to-HA-MSA' = [],
	 'MIP-MN-to-FA-MSA' = [], 'MIP-HA-to-MN-MSA' = [],
	 'MIP-HA-to-FA-MSA' = [], 'MIP-MSA-Lifetime' = [],
	 'MIP-Originating-Foreign-AAA' = [],
	 'MIP-Mobile-Node-Address' = [],
	 'MIP-Home-Agent-Address' = [], 'MIP-Filter-Rule' = [],
	 'Origin-State-Id' = [], 'Proxy-Info' = [],
	 'Route-Record' = [], 'AVP' = []}).

-record(rfc4004_mip_HAA,
	{'Session-Id', 'Auth-Application-Id', 'Result-Code',
	 'Origin-Host', 'Origin-Realm',
	 'Acct-Multi-Session-Id' = [], 'User-Name' = [],
	 'Error-Reporting-Host' = [], 'Error-Message' = [],
	 'MIP-Reg-Reply' = [], 'MIP-Home-Agent-Address' = [],
	 'MIP-Mobile-Node-Address' = [], 'MIP-FA-to-HA-SPI' = [],
	 'MIP-FA-to-MN-SPI' = [], 'Origin-State-Id' = [],
	 'Proxy-Info' = [], 'AVP' = []}).


%%% -------------------------------------------------------
%%% Grouped AVP records:
%%% -------------------------------------------------------

-record('rfc4004_mip_MIP-MN-AAA-Auth',
	{'MIP-MN-AAA-SPI', 'MIP-Auth-Input-Data-Length',
	 'MIP-Authenticator-Length', 'MIP-Authenticator-Offset',
	 'AVP' = []}).

-record('rfc4004_mip_MIP-Originating-Foreign-AAA',
	{'Origin-Realm', 'Origin-Host', 'AVP' = []}).

-record('rfc4004_mip_MIP-Home-Agent-Host',
	{'Destination-Realm', 'Destination-Host', 'AVP' = []}).

-record('rfc4004_mip_MIP-FA-to-MN-MSA',
	{'MIP-FA-to-MN-SPI', 'MIP-Algorithm-Type',
	 'MIP-Session-Key', 'AVP' = []}).

-record('rfc4004_mip_MIP-FA-to-HA-MSA',
	{'MIP-FA-to-HA-SPI', 'MIP-Algorithm-Type',
	 'MIP-Session-Key', 'AVP' = []}).

-record('rfc4004_mip_MIP-HA-to-FA-MSA',
	{'MIP-HA-to-FA-SPI', 'MIP-Algorithm-Type',
	 'MIP-Session-Key', 'AVP' = []}).

-record('rfc4004_mip_MIP-HA-to-MN-MSA',
	{'MIP-Algorithm-Type', 'MIP-Replay-Mode',
	 'MIP-Session-Key', 'AVP' = []}).

-record('rfc4004_mip_MIP-MN-to-FA-MSA',
	{'MIP-Algorithm-Type', 'MIP-Nonce', 'AVP' = []}).

-record('rfc4004_mip_MIP-MN-to-HA-MSA',
	{'MIP-Algorithm-Type', 'MIP-Replay-Mode', 'MIP-Nonce',
	 'AVP' = []}).


%%% -------------------------------------------------------
%%% Grouped AVP records from diameter_gen_base_rfc6733:
%%% -------------------------------------------------------

-record('rfc4004_mip_Proxy-Info',
	{'Proxy-Host', 'Proxy-State', 'AVP' = []}).

-record('rfc4004_mip_Failed-AVP', {'AVP' = []}).

-record('rfc4004_mip_Experimental-Result',
	{'Vendor-Id', 'Experimental-Result-Code'}).

-record('rfc4004_mip_Vendor-Specific-Application-Id',
	{'Vendor-Id', 'Auth-Application-Id' = [],
	 'Acct-Application-Id' = []}).


%%% -------------------------------------------------------
%%% ENUM Macros:
%%% -------------------------------------------------------

-define('RFC4004_MIP_MIP-ALGORITHM-TYPE_HMAC-SHA-1', 2).
-define('RFC4004_MIP_MIP-REPLAY-MODE_NONE', 1).
-define('RFC4004_MIP_MIP-REPLAY-MODE_TIMESTAMPS', 2).
-define('RFC4004_MIP_MIP-REPLAY-MODE_NONCES', 3).



%%% -------------------------------------------------------
%%% DEFINE Macros:
%%% -------------------------------------------------------

-define('RFC4004_MIP_RESULT-CODE_MIP_REPLY_FAILURE', 4005).
-define('RFC4004_MIP_RESULT-CODE_HA_NOT_AVAILABLE', 4006).
-define('RFC4004_MIP_RESULT-CODE_BAD_KEY', 4007).
-define('RFC4004_MIP_RESULT-CODE_MIP_FILTER_NOT_SUPPORTED', 4008).
-define('RFC4004_MIP_RESULT-CODE_NO_FOREIGN_HA_SERVICE', 5024).
-define('RFC4004_MIP_RESULT-CODE_END_TO_END_MIP_KEY_ENCRYPTION', 5025).



%%% -------------------------------------------------------
%%% ENUM Macros from diameter_gen_base_rfc6733:
%%% -------------------------------------------------------

-ifndef('RFC4004_MIP_DISCONNECT-CAUSE_REBOOTING').
-define('RFC4004_MIP_DISCONNECT-CAUSE_REBOOTING', 0).
-endif.
-ifndef('RFC4004_MIP_DISCONNECT-CAUSE_BUSY').
-define('RFC4004_MIP_DISCONNECT-CAUSE_BUSY', 1).
-endif.
-ifndef('RFC4004_MIP_DISCONNECT-CAUSE_DO_NOT_WANT_TO_TALK_TO_YOU').
-define('RFC4004_MIP_DISCONNECT-CAUSE_DO_NOT_WANT_TO_TALK_TO_YOU', 2).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_DONT_CACHE').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_DONT_CACHE', 0).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_SESSION').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_SESSION', 1).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_REALM').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_REALM', 2).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_REALM_AND_APPLICATION').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_REALM_AND_APPLICATION', 3).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_APPLICATION').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_APPLICATION', 4).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_HOST').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_HOST', 5).
-endif.
-ifndef('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_USER').
-define('RFC4004_MIP_REDIRECT-HOST-USAGE_ALL_USER', 6).
-endif.
-ifndef('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY').
-define('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHENTICATE_ONLY', 1).
-endif.
-ifndef('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').
-define('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHORIZE_ONLY', 2).
-endif.
-ifndef('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE').
-define('RFC4004_MIP_AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE', 3).
-endif.
-ifndef('RFC4004_MIP_AUTH-SESSION-STATE_STATE_MAINTAINED').
-define('RFC4004_MIP_AUTH-SESSION-STATE_STATE_MAINTAINED', 0).
-endif.
-ifndef('RFC4004_MIP_AUTH-SESSION-STATE_NO_STATE_MAINTAINED').
-define('RFC4004_MIP_AUTH-SESSION-STATE_NO_STATE_MAINTAINED', 1).
-endif.
-ifndef('RFC4004_MIP_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').
-define('RFC4004_MIP_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY', 0).
-endif.
-ifndef('RFC4004_MIP_RE-AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE').
-define('RFC4004_MIP_RE-AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE', 1).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_LOGOUT').
-define('RFC4004_MIP_TERMINATION-CAUSE_LOGOUT', 1).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_SERVICE_NOT_PROVIDED').
-define('RFC4004_MIP_TERMINATION-CAUSE_SERVICE_NOT_PROVIDED', 2).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_BAD_ANSWER').
-define('RFC4004_MIP_TERMINATION-CAUSE_BAD_ANSWER', 3).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_ADMINISTRATIVE').
-define('RFC4004_MIP_TERMINATION-CAUSE_ADMINISTRATIVE', 4).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_LINK_BROKEN').
-define('RFC4004_MIP_TERMINATION-CAUSE_LINK_BROKEN', 5).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_AUTH_EXPIRED').
-define('RFC4004_MIP_TERMINATION-CAUSE_AUTH_EXPIRED', 6).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_USER_MOVED').
-define('RFC4004_MIP_TERMINATION-CAUSE_USER_MOVED', 7).
-endif.
-ifndef('RFC4004_MIP_TERMINATION-CAUSE_SESSION_TIMEOUT').
-define('RFC4004_MIP_TERMINATION-CAUSE_SESSION_TIMEOUT', 8).
-endif.
-ifndef('RFC4004_MIP_SESSION-SERVER-FAILOVER_REFUSE_SERVICE').
-define('RFC4004_MIP_SESSION-SERVER-FAILOVER_REFUSE_SERVICE', 0).
-endif.
-ifndef('RFC4004_MIP_SESSION-SERVER-FAILOVER_TRY_AGAIN').
-define('RFC4004_MIP_SESSION-SERVER-FAILOVER_TRY_AGAIN', 1).
-endif.
-ifndef('RFC4004_MIP_SESSION-SERVER-FAILOVER_ALLOW_SERVICE').
-define('RFC4004_MIP_SESSION-SERVER-FAILOVER_ALLOW_SERVICE', 2).
-endif.
-ifndef('RFC4004_MIP_SESSION-SERVER-FAILOVER_TRY_AGAIN_ALLOW_SERVICE').
-define('RFC4004_MIP_SESSION-SERVER-FAILOVER_TRY_AGAIN_ALLOW_SERVICE', 3).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_EVENT_RECORD').
-define('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_EVENT_RECORD', 1).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_START_RECORD').
-define('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_START_RECORD', 2).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD').
-define('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_INTERIM_RECORD', 3).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_STOP_RECORD').
-define('RFC4004_MIP_ACCOUNTING-RECORD-TYPE_STOP_RECORD', 4).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_DELIVER_AND_GRANT').
-define('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_DELIVER_AND_GRANT', 1).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_GRANT_AND_STORE').
-define('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_GRANT_AND_STORE', 2).
-endif.
-ifndef('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_GRANT_AND_LOSE').
-define('RFC4004_MIP_ACCOUNTING-REALTIME-REQUIRED_GRANT_AND_LOSE', 3).
-endif.

