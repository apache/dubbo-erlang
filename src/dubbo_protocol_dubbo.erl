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
            do_refer(UrlInfo),
            {ok, todo};
        _ ->
            {skip, Acc}
    end.

do_refer(UrlInfo) ->

    ok.


getClients(ProviderUrl) ->
    case new_transport(ProviderUrl) of
        {ok,ConnectionInfoList} ->
            ConnectionList = start_provider_process(HostFlag, 30, ProviderConfig),
            ok;
        {error,Reason} ->
            {error,Reason}
    end.



%%ok = update_connection_info(ProviderConfig#provider_config.interface, HostFlag, ConnectionList, true),


new_transport(ProviderUrl)->
    case dubbo_node_config_util:parse_provider_info(ProviderUrl) of
        {ok, ProviderConfig} ->
            HostFlag = get_host_flag(ProviderConfig),
            case dubbo_provider_consumer_reg_table:get_host_connections(ProviderConfig#provider_config) of
                [] ->
                    case dubbo_exchanger:connect(ProviderUrl,?MODULE) of
                        {ok,ConnectionInfo} ->
                            {ok,[ConnectionInfo]};
                        {error,Reason} ->
                            logger:warning("start client fail ~p ~p",[Reason,HostFlag]),
                            {error,Reason}
                    end;
                ConnectionInfoList ->
                    {ok,ConnectionInfoList}
            end;
        {error, R1} ->
            logger:error("parse provider info error reason ~p", [R1]),
            {error,R1}
    end.




