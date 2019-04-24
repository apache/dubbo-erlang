%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 12:41 AM
%%%-------------------------------------------------------------------
-module(dubbo_traffic_control_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    dubbo_traffic_control:init(),
    dubbo_traffic_control:init(),
    ?assert(true).

goon_test()->
    dubbo_traffic_control:init(),
    ?assertEqual(dubbo_traffic_control:check_goon(key1,2),ok),
    ?assertEqual(dubbo_traffic_control:check_goon(key1,2),ok),
    ?assertEqual(dubbo_traffic_control:check_goon(key1,2),full),
    ?assertEqual(dubbo_traffic_control:check_goon(key1,2),full),
    ?assertEqual(dubbo_traffic_control:decr_count(key1),ok),
    ok.
