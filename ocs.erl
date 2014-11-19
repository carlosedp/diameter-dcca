-module(ocs).

%-export([generate_req/2]).

-export([start/1]).

start(_Type) ->
    Pid = spawn(fun universal_server/0),
        case _Type of
            ocs_intm ->
                Pid ! {become, fun ocs_intm/0}
        end,
    Pid.

universal_server() ->
    receive
       {become, F} ->
           F()
    end.

ocs_intm() ->
    receive
        {From, {initial, EventData}} ->
            {APN, IMSI, MSISDN, Location, SessionId, EventTimestamp, ConsumedResources, ServiceID, RatingGroup} = EventData,
            StartTime = timestamp(EventTimestamp),
            io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",50},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",0},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),
            SimulatedGrantedQuota =  300000,
            ResultCode = 1,
            From ! {ResultCode, SimulatedGrantedQuota},
            ocs_intm();
        {From, {update, EventData}} ->
            {APN, IMSI, MSISDN, Location, SessionId, EventTimestamp, ConsumedResources, ServiceID, RatingGroup} = EventData,
            StartTime = timestamp(EventTimestamp),
            io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",3},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),
            SimulatedGrantedQuota =  300000,
            ResultCode = 1,
            From ! {ResultCode, SimulatedGrantedQuota},
            ocs_intm();
        {From, {terminate, EventData}} ->
            {APN, IMSI, MSISDN, Location, SessionId, EventTimestamp, ConsumedResources, ServiceID, RatingGroup} = EventData,
            StartTime = timestamp(EventTimestamp),
            io:format("[{7,\"OtherParty\",[{5,\"APN\",\"~s\"}]},{3,\"MessageType\",2},{7,\"ServedParty\",[{5,\"IMSI\",\"~s\"},{5,\"MSISDN\",\"~s\"},{5,\"Location\",\"~s\"}]},{5,\"Version\",\"5.00.0\"},{5,\"SessionId\",\"~s\"},{7,\"Resources\",[{7,\"R_1\",[{5,\"StartTime\",\"~s\"},{1,\"ConsumedResources\",~B},{5,\"SubServiceType\",\"~2..0B/~3..0B\"},{3,\"ReportingReason\",2},{1,\"RequestedResources\",-1},{5,\"ServiceType\",\"GPRS:0:~2..0B:~3..0B\"},{3,\"QuotaType\",3},{3,\"Id\",~B~3..0B}]}]}]~n",[APN, IMSI, MSISDN, Location, SessionId, StartTime, ConsumedResources, ServiceID, RatingGroup, ServiceID, RatingGroup, ServiceID, RatingGroup]),
            SimulatedGrantedQuota =  300000,
            ResultCode = 1,
            From ! {ResultCode, SimulatedGrantedQuota},
            ocs_intm()
    end.

timestamp([{{YY, MM, DD}, {Hour, Min, Sec}}]) ->
    io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [YY, MM, DD, Hour, Min, Sec]);
timestamp([]) ->
    {YY, MM, DD} = date(),
    {Hour, Min, Sec} = time(),
    io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [YY, MM, DD, Hour, Min, Sec]).
