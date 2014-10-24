-module(ocs_intm).

-export([generate_req/2]).

%% Query OCS and get session quota
%%
generate_req(initial, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
    io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",50},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",0},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

    SimulatedGrantedQuota =  300000,
    ResultCode = 1,
    {ResultCode, SimulatedGrantedQuota};

generate_req(update, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
    io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",3},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

    SimulatedGrantedQuota =  300000,
    ResultCode = 1,
    {ResultCode, SimulatedGrantedQuota};

generate_req(terminate, {APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup}) ->
    io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",2},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),

    SimulatedGrantedQuota =  300000,
    ResultCode = 1,
    {ResultCode, SimulatedGrantedQuota}.
