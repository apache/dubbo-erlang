%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. May 2018 4:28 PM
%%%-------------------------------------------------------------------
-module(request_context).
-author("dlive").

%%-define(PER_LOG,true).
-define(PER_LOG,false).

%% API
-export([init/0,init/1,update/2,update/3,to_log/1,test/0,trace/1,trace_end/1]).

init()->
    case ?PER_LOG of
        true->
            #{<<"t_b">> => time_util:timestamp_ms()};
        _->
            #{}
    end.
init(ID)->
    case ?PER_LOG of
        true->
            #{<<"id">> =>ID,<<"t_b">> => time_util:timestamp_ms()};
        _->
            #{<<"id">> =>ID}
    end.

update(Key,Map)->
    case ?PER_LOG of
        true->
            Map#{Key=>time_util:timestamp_ms()};
        _->
            Map
    end.

update(Key,Value,Map)->
    Map#{Key=>Value}.

to_log(Map)->
    case ?PER_LOG of
        true->
            to_log2(Map);
        _->
            ok
    end.

to_log2(Map)->

    #{
        <<"t_b">> := Begin,
        <<"t_lb">> := LoadBalance,
        <<"t_send_agent">> := SendAgent,
        <<"t_agent_b">> := AgentBegin,
        <<"t_agent_e">> := AgentEnd,
        <<"t_net_b">> := NetBegin,
%%        <<"t_net_bd">> := NetBeginDecode,
        <<"t_net_e">> := NetEnd,
        <<"t_backer_b">> := BackerBegin,
        <<"t_backer_e1">> := BackerGet,
        <<"t_backer_e">> := BackerEnd,
        <<"t_gate_back">> := GateWayBack,
%%      <<"t_e">> := End,
        <<"t_nd">> :=Node
    } = Map,
    Total=GateWayBack-Begin,
    if
        Total > 90 ->
            performance:warning("total ~-5.10B lb ~-5.10B to_send ~-5.10B agent_b ~-5.10B agent_e ~-5.10B net_b ~-5.10B net_e ~-5.10B backer_b ~-5.10B backer_e1 ~-5.10B backer_e ~-5.10B gw_b ~-5.10B ~p",
                [
                    Total,
                    LoadBalance - Begin,
                    SendAgent-Begin,
                    AgentBegin-SendAgent,
                    AgentEnd-AgentBegin,
                    NetBegin-AgentEnd,
                    NetEnd-NetBegin,
                    BackerBegin-NetEnd,
                    BackerGet-BackerBegin,
                    BackerEnd-BackerBegin,
                    GateWayBack-BackerEnd,
%%                    End-GateWayBack,
                    Node
                ]);
        true->
            performance:info("total ~-5.10B lb ~-5.10B to_send ~-5.10B agent_b ~-5.10B agent_e ~-5.10B net_b ~-5.10B net_e ~-5.10B backer_b ~-5.10B backer_e1 ~-5.10B backer_e ~-5.10B gw_b ~-5.10B ~p",
                [
                    Total,
                    LoadBalance - Begin,
                    SendAgent-Begin,
                    AgentBegin-SendAgent,
                    AgentEnd-AgentBegin,
                    NetBegin-AgentEnd,
                    NetEnd-NetBegin,
                    BackerBegin-NetEnd,
                    BackerGet-BackerBegin,
                    BackerEnd-BackerBegin,
                    GateWayBack-BackerEnd,
%%                    End-GateWayBack,
                    Node
                ])
    end.

trace(Info)->
%%    gen_server:cast({monitor_trace,'monitor@agent_monitor.me'},{trace,Info}).
    ok.

trace_end(Info)->
%%    gen_server:cast({monitor_trace,'monitor@agent_monitor.me'},{trace_end,Info}).
    ok.

test()->
    lager:error("a~-10.5sb",[123]).