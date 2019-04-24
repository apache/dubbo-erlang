%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dec 2017 4:55 PM
%%%-------------------------------------------------------------------
-module(dubbo_zookeeper_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").
exist_test() ->
    Consumer=#consumer_config{interface = <<"com.ifcoder.demo.facade.User">>,
        methods = [<<"a">>,<<"b">>]},
    V= dubbo_zookeeper:gen_consumer_node_info(Consumer),
    ?debugFmt("consumer info ~p",[V]),
    ?assert(is_binary(V)).
