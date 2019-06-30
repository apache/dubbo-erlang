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
-module(dubbo_protocol_registry).
-behaviour(dubbo_protocol).

-include("dubboerl.hrl").
-include("dubbo.hrl").
-include("constrans.hrl").


%% API
-export([refer/2, export/2, destroy/0]).

refer(Url, Acc) ->
    {ok, UrlInfo} = dubbo_common_fun:parse_url(Url),
    RegistryUrlInfo = gen_registry_urlinfo(UrlInfo),
    {ok, RegistryName} = dubbo_registry:setup_register(RegistryUrlInfo),

    ConsumerUrl = gen_consumer_url(UrlInfo),
    dubbo_registry:register(RegistryName, ConsumerUrl),

    dubbo_directory:subscribe(RegistryName, ConsumerUrl),

    ok.

export(Invoker, Acc) ->
    {ok, UrlInfo} = dubbo_common_fun:parse_url(Invoker#invoker.url),
    %% url = registry://127.0.0.1:2181/org.apache.dubbo.registry.RegistryService?application=hello-world&dubbo=2.0.2&export=dubbo%3A%2F%2F192.168.1.5%3A20880%2Forg.apache.dubbo.erlang.sample.service.facade.UserOperator%3Fanyhost%3Dtrue%26application%3Dhello-world%26bean.name%3Dorg.apache.dubbo.erlang.sample.service.facade.UserOperator%26bind.ip%3D192.168.1.5%26bind.port%3D20880%26default.deprecated%3Dfalse%26default.dynamic%3Dfalse%26default.register%3Dtrue%26deprecated%3Dfalse%26dubbo%3D2.0.2%26dynamic%3Dfalse%26generic%3Dfalse%26interface%3Dorg.apache.dubbo.erlang.sample.service.facade.UserOperator%26methods%3DqueryUserInfo%2CqueryUserList%2CgenUserId%2CgetUserInfo%26pid%3D11272%26register%3Dtrue%26release%3D2.7.1%26side%3Dprovider%26timestamp%3D1563110211090&pid=11272&registry=zookeeper&release=2.7.1&timestamp=1563110211064
    ProtocolUrl = get_provider_url(UrlInfo),
    {ok, InterfaceKey} = do_local_export(Invoker, ProtocolUrl),

    RegistryUrlInfo = gen_registry_urlinfo(UrlInfo),
    {ok, RegistryName} = dubbo_registry:setup_register(RegistryUrlInfo),
    dubbo_registry:register(RegistryName, ProtocolUrl),

    register_export_info(ProtocolUrl, RegistryName, InterfaceKey),
    {ok, Invoker}.

destroy() ->
    List = ets:tab2list(?SERVICE_EXPORT_TABLE),
    lists:map(
        fun(Item) ->

            {ProtocolUrl, RegistryModule, _} = Item,
            io:format(user, "destroy url ~p~n", [ProtocolUrl]),
            unexport(RegistryModule, ProtocolUrl)
        end, List),
    ok.

unexport(RegistryModule, Url) ->
    dubbo_registry:unregister(RegistryModule, Url),
    ok.

do_local_export(Invoker, Url) ->
    %% Url = dubbo://127.0.0.1:20880/org.apache.dubbo.erlang.sample.service.facade.UserOperator?anyhost=true&application=hello-world&bean.name=org.apache.dubbo.erlang.sample.service.facade.UserOperator&bind.ip=127.0.0.1&bind.port=20880&default.deprecated=false&default.dynamic=false&default.register=true&deprecated=false&dubbo=2.0.2&dynamic=false&generic=false&interface=org.apache.dubbo.erlang.sample.service.facade.UserOperator&methods=queryUserInfo,queryUserList,genUserId,getUserInfo&pid=90956&register=true&release=2.7.1&side=provider&timestamp=1562725983984
    {ok, UrlInfo} = dubbo_common_fun:parse_url(Url),
    Protocol = UrlInfo#dubbo_url.scheme,
    ProtocolModule = binary_to_existing_atom(<<<<"dubbo_protocol_">>/binary, Protocol/binary>>, latin1),
    _Result = apply(ProtocolModule, export, [Invoker#invoker{url = Url}, ok]),

    InterfaceKey = maps:get(<<"interface">>, UrlInfo#dubbo_url.parameters),

    {ok, InterfaceKey}.

register_export_info(ProtocolUrl, RegistryModule, InterfaceKey) ->
    ets:insert(?SERVICE_EXPORT_TABLE, {ProtocolUrl, RegistryModule, InterfaceKey}),
    ok.


gen_consumer_url(UrlInfo) ->
    Parameters = UrlInfo#dubbo_url.parameters,
    #{<<"refer">> := Refer} = Parameters,
    Refer2 = http_uri:decode(Refer),
    Parameters2 = dubbo_common_fun:parse_url_parameter(Refer2),
    Parameters3 = Parameters2#{
        ?CATEGORY_KEY => ?CONSUMERS_CATEGORY
    },
    #{<<"interface">> := Interface} = Parameters3,
    ConsumerUrlInfo = UrlInfo#dubbo_url{
        scheme = <<"consumer">>,
        host = dubbo_common_fun:local_ip_v4_str(),
        path = Interface,
        parameters = Parameters3
    },
    ConsumerUrl = dubbo_common_fun:url_to_binary(ConsumerUrlInfo),
    ConsumerUrl.
get_provider_url(UrlInfo) ->
    ExportUrl = maps:get(<<"export">>, UrlInfo#dubbo_url.parameters),
    ExportUrl2 = http_uri:decode(ExportUrl),
    {ok,ExportUrlInfo} = dubbo_common_fun:parse_url(ExportUrl2),
    ParameterNew = maps:put(?CATEGORY_KEY,?PROVIDERS_CATEGORY,ExportUrlInfo#dubbo_url.parameters),
    ExportUrlInfoNew = ExportUrlInfo#dubbo_url{parameters = ParameterNew},
    logger:debug("registry gen provider url info ~p",[ExportUrlInfoNew]),
    dubbo_common_fun:url_to_binary(ExportUrlInfoNew).

gen_registry_urlinfo(UrlInfo) ->
    Parameters = UrlInfo#dubbo_url.parameters,
    UrlInfo#dubbo_url{
        scheme = maps:get(<<"registry">>, Parameters, <<"zookeeper">>)
    }.