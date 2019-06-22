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
-module(dubbo_protocol_dubbo).

-include("dubboerl.hrl").
-include("dubbo.hrl").

%% API
-export([refer/2]).

refer(Url, Acc) ->
    {ok, UrlInfo} = dubbo_common_fun:parse_url(Url),
    case UrlInfo#dubbo_url.scheme of
        <<"dubbo">> ->
            {ok,Invoker} = do_refer(UrlInfo),
            {ok, Invoker};
        _ ->
            {skip, Acc}
    end.

do_refer(UrlInfo) ->
    case dubbo_node_config_util:parse_provider_info(UrlInfo) of
        {ok, ProviderConfig} ->
%%            OldHostList = dubbo_provider_consumer_reg_table:get_interface_provider_node(ProviderConfig#provider_config.interface),
            case getClients(ProviderConfig) of
                {ok, ConnectionInfoList} ->
                    dubbo_provider_consumer_reg_table:update_node_conections(ProviderConfig#provider_config.interface,ConnectionInfoList),
                    HostFlag = dubbo_provider_consumer_reg_table:get_host_flag(ProviderConfig),
                    {ok,#dubbo_invoker{host_flag = HostFlag,handle = ?MODULE}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, R1} ->
            logger:error("parse provider info error reason ~p", [R1]),
            {error, R1}
    end.

getClients(ProviderConfig) ->
    %% @todo if connections parameter > 1, need new spec transport
    case new_transport(ProviderConfig) of
        {ok, ConnectionInfoList} ->
%%            ConnectionList = start_provider_process(HostFlag, 30, ProviderConfig),
            {ok, ConnectionInfoList};
        {error, Reason} ->
            {error, Reason}
    end.


%%ok = update_connection_info(ProviderConfig#provider_config.interface, HostFlag, ConnectionList, true),


new_transport(ProviderConfig) ->

    HostFlag = get_host_flag(ProviderConfig),
    case dubbo_provider_consumer_reg_table:get_host_connections(ProviderConfig#provider_config) of
        [] ->
            case dubbo_exchanger:connect(ProviderConfig, ?MODULE) of
                {ok, ConnectionInfo} ->
                    {ok, [ConnectionInfo]};
                {error, Reason} ->
                    logger:warning("start client fail ~p ~p", [Reason, HostFlag]),
                    {error, Reason}
            end;
        ConnectionInfoList ->
            {ok, ConnectionInfoList}
    end.




