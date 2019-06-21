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
-module(dubbo_exchanger).

-include("dubbo.hrl").

%% API
-export([connect/2]).

connect(Url,Handler) ->
    case dubbo_node_config_util:parse_provider_info(Url) of
        {ok, ProviderConfig} ->
            HostFlag= dubbo_provider_consumer_reg_table:get_host_flag(ProviderConfig),
            {ok, Pid} = dubbo_transport_pool_sup:add_children(ProviderConfig,Handler),
            logger:info("start provider ~p pid info ~p~n", [HostFlag, Pid]),
            {ok,#connection_info{ pid = Pid, weight = get_weight(ProviderConfig), host_flag = HostFlag}};
        {error, R1} ->
            logger:error("parse provider info error reason ~p", [R1]),
            {error,R1}
    end.



get_weight(_ProviderConfig)->
    %% todo get weight from provider info
    30.