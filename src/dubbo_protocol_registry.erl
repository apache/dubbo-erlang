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

%% API
-export([]).

refer(InterfaceClassInfo,Url)->
    {ok,UrlInfo} =  dubbo_common_fun:parse_url(Url),

    {ok,RegistryName} = dubbo_registry:setup_register(UrlInfo),

    ConsumerUrl = gen_consumer_url(UrlInfo),
    %% 通知directory
    dubbo_registry:register(RegistryName,ConsumerUrl),

    dubbo_directory:subscribe(RegistryName,ConsumerUrl),

    %% return
    ok.


gen_consumer_url(UrlInfo)->
    Parameters = UrlInfo#dubbo_url.parameters,
    #{<<"refer">> := Refer} = Parameters,
    Refer2 = http_uri:decode(Refer),
    Parameters2 = dubbo_common_fun:parse_url(Refer2,#{}),
    #{<<"interface">> := Interface} = Parameters2,
    ConsumerUrlInfo = UrlInfo#dubbo_url{
        scheme = <<"consumer">>,
        host = dubbo_common_fun:local_ip_v4_str(),
        path = Interface,
        parameters = Parameters2
    },
    ConsumerUrl = dubbo_common_fun:map_to_url(ConsumerUrlInfo),
    ConsumerUrl.