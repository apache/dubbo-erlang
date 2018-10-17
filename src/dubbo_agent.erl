%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2018 12:15 AM
%%%-------------------------------------------------------------------
-module(dubbo_agent).
-author("dlive").

%% API
-export([register_consumer/1]).

register_consumer(Interface)->
    ProviderList=[
        "dubbo%3a%2f%2f127.0.0.1%3a20880%2fcom.alibaba.dubbo.performance.demo.provider.IHelloService%3fanyhost%3dtrue%26application%3dmesh-provider%26dubbo%3d2.6%26executes%3d30%26interface%3dcom.alibaba.dubbo.performance.demo.provider.IHelloService%26methods%3dhash%26pid%3d26029%26side%3dprovider%26timestamp%3d1514615143388"
    ],
    dubbo_consumer_pool:start_consumer(Interface,ProviderList),
    ok.
