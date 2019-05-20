-module(dubbo_sample_service_type_list).

%% API
-export([register_type_list/0,get_list/0]).

-include("dubbo_sample_service.hrl").

get_list()->
    [
        {userInfoRequest,<<"org.apache.dubbo.erlang.sample.service.bean.UserInfoRequest">>,record_info(fields,userInfoRequest)},
        {userRes,<<"org.apache.dubbo.erlang.sample.service.bean.UserRes">>,record_info(fields,userRes)},
        {userInfo,<<"org.apache.dubbo.erlang.sample.service.bean.UserInfo">>,record_info(fields,userInfo)}    ].

register_type_list()->
    ok.