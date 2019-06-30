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
-module(dubbo_config_util).

-include("dubbo.hrl").
%% API
-export([gen_consumer/3, gen_provider/6]).


gen_consumer(Application, Interface, Option) ->
    #consumer_config{
        interface = Interface,
        application = Application,
        category = <<"consumers">>,
        check = false,
        default_timeout = proplists:get_value(default_timeout, Option, 500),
        dubbo_version = proplists:get_value(dubbo_version, Option, ?DUBBO_VERSION),
        methods = [],
        revision = <<"">>,
        side = <<"consumers">>
    }.

gen_provider(Application, Port, Interface, MethodList, ImplModuleName, _Option) ->
    Host = dubbo_network_tools:local_ipv4_binary(),
    MethodList2 = [atom_to_binary(Item, utf8) || Item <- MethodList],
    #provider_config{
        protocol = <<"dubbo">>,
        host = Host,
        port = Port,
        interface = Interface,
        anyhost = true,
        executes = 10,
        application = Application,
        methods = MethodList2,
        side = <<"provider">>,
        impl_handle = ImplModuleName
    }.