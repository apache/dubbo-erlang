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
-module(dubbo_service_config).

-include("dubbo.hrl").
-include("dubboerl.hrl").
%% API
-export([export/1]).

-spec(export(#provider_config{}) -> ok).
export(ProviderInfo) ->
    logger:debug("will export provider info ~p", [ProviderInfo]),
    do_export(ProviderInfo),
    ok.

do_export(ProviderInfo) ->
    do_export_protocol(ProviderInfo),
    ok.

do_export_protocol(ProviderInfo) ->
    Url = get_registry_url(ProviderInfo),
    logger:debug("do export protocol for url ~p", [Url]),
    Invoker = #invoker{url = Url, handler = ProviderInfo#provider_config.impl_handle},
    dubbo_extension:run_fold(protocol_wapper, export, [Invoker], ok),
    ok.




get_registry_url(ProviderInfo) ->
    {Host, Port} = dubbo_registry:get_registry_host_port(),
    UrlInfo = #dubbo_url{
        scheme = <<"registry">>,
        host = list_to_binary(Host),
        port = Port,
        path = <<"org.apache.dubbo.registry.RegistryService">>,
        parameters = gen_registry_parameter(ProviderInfo)
    },
    dubbo_common_fun:url_to_binary(UrlInfo).

gen_registry_parameter(ProviderInfo) ->
    Para = #{
        <<"application">> => ProviderInfo#provider_config.application,
        <<"dubbo">> => <<"2.0.2">>,
        <<"pid">> => list_to_binary(os:getpid()),
        <<"export">> => get_export_info(ProviderInfo),
        <<"registry">> => dubbo_registry:get_registry_type(),
        <<"release">> => <<"2.7.1">>,
        <<"timestamp">> => integer_to_binary(dubbo_time_util:timestamp_ms())
    },
    Para.

get_export_info(ProviderInfo) ->
    %%dubbo://127.0.0.1:20880/org.apache.dubbo.erlang.sample.service.facade.UserOperator?
    %% anyhost=true&
    %% application=hello-world&
    %% bean.name=org.apache.dubbo.erlang.sample.service.facade.UserOperator&
    %% bind.ip=127.0.0.1&bind.port=20880&default.deprecated=false&
    %% default.dynamic=false&default.register=true&deprecated=false&dubbo=2.0.2&
    %% dynamic=false&generic=false&
    %% interface=org.apache.dubbo.erlang.sample.service.facade.UserOperator&
    %% methods=queryUserInfo,queryUserList,genUserId,getUserInfo&pid=90956&register=true&release=2.7.1&side=provider&timestamp=1562725983984
    Para = [
        {"anyhost", "true"},
        {"application", ProviderInfo#provider_config.application},
        {"bean.name", ProviderInfo#provider_config.interface},
        {"bind.ip", dubbo_common_fun:local_ip_v4_str()},
        {"bind.port", integer_to_list(ProviderInfo#provider_config.port)},
        {"default.deprecated", "false"},
        {"default.dynamic", "false"},
        {"default.register", "true"},
        {"deprecated", "false"},
        {"dynamic", "false"},
        {"generic", "false"},
        {"interface", ProviderInfo#provider_config.interface},
        {"methods", format_methods_str(ProviderInfo#provider_config.methods)},
        {"pid", os:getpid()},
        {"register", "true"},
        {"release", "2.7.1"},
        {"side", "provider"},
        {"dubbo", "2.0.2"},
        {"timestamp", integer_to_list(dubbo_time_util:timestamp_ms())}
    ],
    UrlInfo = #dubbo_url{
        scheme = ProviderInfo#provider_config.protocol,
        host = dubbo_common_fun:local_ip_v4_str(),
        port = ProviderInfo#provider_config.port,
        path = ProviderInfo#provider_config.interface,
        parameters = Para
    },
    Url = dubbo_common_fun:url_to_binary(UrlInfo),
    list_to_binary(http_uri:encode(binary_to_list(Url))).

format_methods_str(Methods) ->
    Methods2 = [binary_to_list(Item) || Item <- Methods],
    string:join(Methods2, ",").