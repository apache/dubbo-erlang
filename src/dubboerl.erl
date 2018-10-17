%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2017 11:54 PM
%%%-------------------------------------------------------------------
-module(dubboerl).
-author("dlive").

-include("dubboerl.hrl").
%% API
-export([init/0,start_consumer/0,start_provider/0]).

init()->
    start_consumer(),
    start_provider(),
    ok.


start_consumer()->
    ConsumerList = application:get_env(dubboerl,consumer,[]),
    ApplicationName = application:get_env(dubboerl,application,<<"defaultApplication">>),
    lists:map(fun({Interface,Option})->
        ConsumerInfo = dubbo_config_util:gen_consumer(ApplicationName,Interface,Option),
        dubbo_zookeeper:register_consumer(ConsumerInfo),
        lager:info("register consumer success ~p",[Interface])
        end,ConsumerList),
    ok.

start_provider()->
    ProviderList = application:get_env(dubboerl,provider,[]),
    ApplicationName = application:get_env(dubboerl,application,<<"defaultApplication">>),
    DubboServerPort = application:get_env(dubboerl,port,20881),
    start_provider_listen(DubboServerPort),
    lists:map(fun({ImplModuleName,BehaviourModuleName,Interface,Option})->
        ok = dubbo_provider_protocol:register_impl_provider(Interface,ImplModuleName,BehaviourModuleName),
        MethodList= apply(BehaviourModuleName,get_method_999_list,[]),
        ProviderInfo = dubbo_config_util:gen_provider(ApplicationName,DubboServerPort,Interface,MethodList,Option),
        dubbo_zookeeper:register_provider(ProviderInfo),
        lager:info("register provider success ~p ~p",[ImplModuleName,Interface])
        end,ProviderList),
    ok.

start_provider_listen(Port)->
    ets:new(?PROVIDER_IMPL_TABLE,[public,named_table]),
    {ok, _} = ranch:start_listener(tcp_reverse,
        ranch_tcp, [{port, Port}], dubbo_provider_protocol, []),
    ok.




