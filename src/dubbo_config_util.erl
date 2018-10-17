%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2018 12:21 AM
%%%-------------------------------------------------------------------
-module(dubbo_config_util).
-author("dlive").

-include("dubbo.hrl").
%% API
-export([gen_consumer/3,gen_provider/5]).


gen_consumer(Application,Interface,Option)->
    #consumer_config{
        interface = Interface,
        application = Application,
        category = <<"consumers">> ,
        check=false,
        default_timeout = proplists:get_value(default_timeout,Option,500),
        dubbo_version= proplists:get_value(dubbo_version,Option,?DUBBO_VERSION),
        methods=[],
        revision= <<"">>,
        side= <<"consumers">>
    }.

gen_provider(Application,Port,Interface,MethodList,Option)->
    Host = de_network_tools:local_ipv4_binary(),
    MethodList2= [atom_to_binary(Item,utf8) || Item <- MethodList ],
    #provider_config{
        protocol= <<"dubbo">>,
        host= Host,
        port = Port,
        interface=Interface,
        anyhost=true,
        executes=10,
        application=Application,
        methods = MethodList2,
        side= <<"provider">>
    }.