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

gen_provice_config_test()->
    ProviderConfigInfo = dubbo_config_util:gen_provider(<<"defaultApp">>,20880,<<"org.apache.dubbo.test.interface">>,[method1],[]),
    ProvideNode = dubbo_node_config_util:gen_provider_info(ProviderConfigInfo),
    ?assert(is_binary(ProvideNode)).


provider_parse_test() ->
    {ok,ProviderConfig} = dubbo_node_config_util:parse_provider_info(<<"dubbo%3A%2F%2F127.0.0.1%3A20880%2Forg.apache.dubbo.test.interface%3Finterface=org.apache.dubbo.test.interface&application=defaultApp&anyhost=true&dubbo=2.5.3&executes=10&methods=method1&side=provider&timestamp=1556095933071">>),
    ?assertEqual(ProviderConfig#provider_config.protocol,dubbo),
    ?assertEqual(ProviderConfig#provider_config.host,"127.0.0.1"),
    ?assertEqual(ProviderConfig#provider_config.port,20880),
    ?assertEqual(ProviderConfig#provider_config.interface,<<"org.apache.dubbo.test.interface">>),
    ?assert(true).

