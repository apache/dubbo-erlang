-module(dubbo_service_type_list).

%% API
-export([register_type_list/0,get_list/0]).

-include("dubbo_service.hrl").

get_list()->
    [
        {userInfoRequest,<<"com.ifcoder.demo.bean.UserInfoRequest">>,record_info(fields,userInfoRequest)},
        {list,<<"java.util.List">>,record_info(fields,list)},
        {userInfo,<<"com.ifcoder.demo.bean.UserInfo">>,record_info(fields,userInfo)},
        {userRes,<<"com.ifcoder.demo.bean.UserRes">>,record_info(fields,userRes)}    ].

register_type_list()->
    ok.