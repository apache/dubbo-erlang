%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Dec 2017 2:52 PM
%%%-------------------------------------------------------------------
-module(dubbo_config_parser_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").

simple_test() ->
    {ok,ProviderConfig} = dubbo_node_config_util:parse_provider_info("..."),
%%    io:format(user,"parse config result ~p~n",[ProviderConfig]),
    ?assertEqual(ProviderConfig#provider_config.protocol,dubbo),
    ?assertEqual(ProviderConfig#provider_config.host,"192.168.1.6"),
    ?assertEqual(ProviderConfig#provider_config.port,20880),
    ?assertEqual(ProviderConfig#provider_config.interface,<<"...">>),
    ?assert(true).
