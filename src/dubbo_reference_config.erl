%%------------------------------------------------------------------------------
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed with
%% this work for additional information regarding copyright ownership.
%% The ASF licenses this file to You under the Apache License, Version 2.0
%% (the "License"); you may not use this file except in compliance with
%% the License.  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------
-module(dubbo_reference_config).

-include("dubbo.hrl").
-include("dubboerl.hrl").

-record(dubbo_interface_info, {}).

%% API
-export([init_reference/1]).

init_reference(ConsumerInfo) ->
%%    InitConfigMap= #{
%%
%%    },
    %% 组装各类需要数据
    create_proxy(ConsumerInfo),
    ok.


create_proxy(ConsumerInfo) ->

    Para = gen_parameter(ConsumerInfo),
    Url = gen_registry_url(Para),
    ok = dubbo_extension:run_fold(protocol_wapper, refer, [Url], ok),
    ok.

%%application=hello-world&dubbo=2.0.2&pid=68901&refer=application=hello-world&default.check=false&default.lazy=false&default.retries=0&default.sticky=false&default.timeout=300000&dubbo=2.0.2&interface=org.apache.dubbo.erlang.sample.service.facade.UserOperator&lazy=false&methods=queryUserInfo,queryUserList,genUserId,getUserInfo&pid=68901&register.ip=127.0.0.1&release=2.7.1&retries=0&side=consumer&sticky=false&timestamp=1559727789953&registry=zookeeper&release=2.7.1&timestamp=1559727842451


gen_registry_url(Para) ->
    %%todo 组装para & url
    {Host, Port} = dubbo_registry:get_registry_host_port(),
    UrlInfo = #dubbo_url{
        scheme = <<"registry">>,
        host = list_to_binary(Host),
        port = Port,
        path = <<"org.apache.dubbo.registry.RegistryService">>,
        parameters = Para
    },
    dubbo_common_fun:url_to_binary(UrlInfo).
%%    Url = "registry://127.0.0.1:2181/org.apache.dubbo.registry.RegistryService?application=hello-world&dubbo=2.0.2&pid=68901&refer=application%3Dhello-world%26default.check%3Dfalse%26default.lazy%3Dfalse%26default.retries%3D0%26default.sticky%3Dfalse%26default.timeout%3D300000%26dubbo%3D2.0.2%26interface%3Dorg.apache.dubbo.erlang.sample.service.facade.UserOperator%26lazy%3Dfalse%26methods%3DqueryUserInfo%2CqueryUserList%2CgenUserId%2CgetUserInfo%26pid%3D68901%26register.ip%3D127.0.0.1%26release%3D2.7.1%26retries%3D0%26side%3Dconsumer%26sticky%3Dfalse%26timestamp%3D1559727789953&registry=zookeeper&release=2.7.1&timestamp=1559727842451",
%%    Url.


gen_parameter(ConsumerInfo) ->
    Para = #{
        <<"application">> => get_appname(ConsumerInfo),
        <<"dubbo">> => <<"2.0.2">>,
        <<"pid">> => list_to_binary(get_pid()),
        <<"refer">> => get_refinfo(ConsumerInfo),
        <<"registry">> => dubbo_registry:get_registry_type(),
        <<"release">> => <<"2.7.1">>,
        <<"timestamp">> => integer_to_binary(dubbo_time_util:timestamp_ms())
    },
    Para.

get_appname(ConsumerInfo) ->
    ConsumerInfo#consumer_config.application.
get_pid() ->
    os:getpid().
get_refinfo(ConsumerInfo) ->
    KeyValues = [
        {"application", ConsumerInfo#consumer_config.application},
        {"default.check", atom_to_list(ConsumerInfo#consumer_config.check)},
        {"default.lazy", "false"},
        {"default.retries", "0"},
        {"default.sticky", "false"},
        {"default.timeout", "300000"},
        {"dubbo", "2.0.2"},
        {"interface", ConsumerInfo#consumer_config.interface},
        {"lazy", "false"},
        {"methods", string:join(ConsumerInfo#consumer_config.methods, ",")},
        {"register.ip", ConsumerInfo#consumer_config.application},
        {"release", "2.7.1"},
        {"pid", get_pid()},
        {"side", "consumer"},
        {"sticky", "false"},
        {"timestamp", integer_to_list(dubbo_time_util:timestamp_ms())}
    ],
    KeyValues2 = [io_lib:format("~s=~s", [Key, Value]) || {Key, Value} <- KeyValues],
    ParameterStr1 = string:join(KeyValues2, "&"),
    list_to_binary(http_uri:encode(ParameterStr1)).
%%    <<"application%3Dhello-world%26default.check%3Dfalse%26default.lazy%3Dfalse%26default.retries%3D0%26default.sticky%3Dfalse%26default.timeout%3D300000%26dubbo%3D2.0.2%26interface%3Dorg.apache.dubbo.erlang.sample.service.facade.UserOperator%26lazy%3Dfalse%26methods%3DqueryUserInfo%2CqueryUserList%2CgenUserId%2CgetUserInfo%26pid%3D68901%26register.ip%3D127..0.1%26release%3D2.7.1%26retries%3D0%26side%3Dconsumer%26sticky%3Dfalse%26timestamp%3D1559727789953">>.


