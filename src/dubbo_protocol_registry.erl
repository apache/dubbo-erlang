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
    case dubbo_registry:register(RegistryName, ConsumerUrl) of
        {ok,Result}->
            logger:info("registry ~p register success ~p",[RegistryName,Result]);
        {error,Reason1} ->
            logger:warning("registry ~p register fail ~p ~p",[RegistryName,Reason1,ConsumerUrl])
    end,
    dubbo_directory:subscribe(RegistryName, ConsumerUrl),
    {ok,Acc}.

export(Invoker, Acc) ->
    {ok, UrlInfo} = dubbo_common_fun:parse_url(Invoker#invoker.url),
    ProtocolUrl = get_provider_url(UrlInfo),
    {ok, InterfaceKey} = do_local_export(Invoker, ProtocolUrl),

    RegistryUrlInfo = gen_registry_urlinfo(UrlInfo),
    {ok, RegistryName} = dubbo_registry:setup_register(RegistryUrlInfo),
    case dubbo_registry:register(RegistryName, ProtocolUrl) of
        {ok,Result}->
            logger:info("registry ~p register success ~p",[RegistryName,Result]);
        {error,Reason1} ->
            logger:warning("registry ~p register fail ~p ~p",[RegistryName,Reason1,ProtocolUrl])
    end,
    register_export_info(ProtocolUrl, RegistryName, InterfaceKey),
    {ok, Acc}.

destroy() ->
    List = ets:tab2list(?SERVICE_EXPORT_TABLE),
    lists:map(
        fun(Item) ->
            {ProtocolUrl, RegistryModule, _} = Item,
            unexport(RegistryModule, ProtocolUrl)
        end, List),
    ok.

unexport(RegistryModule, Url) ->
    dubbo_registry:unregister(RegistryModule, Url),
    ok.

do_local_export(Invoker, Url) ->
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