%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2018 1:58 PM
%%%-------------------------------------------------------------------
-module(dubbo_traffic_control).
-author("dlive").
-include("dubboerl.hrl").
%% API
-export([init/0,check_goon/2,decr_count/1]).


init()->
    case ets:info(?TRAFFIC_CONTROL) of
        undefined ->
            io:format("init decoding TRAFFIC_CONTROL table pid ~p~n",[self()]),
            ets:new(?TRAFFIC_CONTROL,[public,named_table, {write_concurrency, true}]); %% public
        _ ->
            ets:delete(?TRAFFIC_CONTROL),
            ets:new(?TRAFFIC_CONTROL,[public,named_table, {write_concurrency, true}])
    end,
    ok.


check_goon(Key,Max)->
    try ets:update_counter(?TRAFFIC_CONTROL,Key,1) of
        Value when Value > Max ->
            ets:update_counter(?TRAFFIC_CONTROL,Key,-1),
            full;
        _V ->
%%            logger:debug("check traffic incr value ~p",[V]),
            ok

    catch
        _T:_R->
            ets:insert(?TRAFFIC_CONTROL,{Key,1}),
            ok
    end.

decr_count(Key)->
    try ets:update_counter(?TRAFFIC_CONTROL,Key,-1) of
        _V ->
            ok
    catch
        _T:_R->
            ets:insert(?TRAFFIC_CONTROL,{Key,0}),
            ok
    end.