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
-module(dubbo_config_parser_tests).
-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").

gen_provice_config_test() ->
    ProviderConfigInfo = dubbo_config_util:gen_provider(<<"defaultApp">>, 20880, <<"org.apache.dubbo.test.interface">>, [method1],dubbo_service_user_impl, []),
    ProvideNode = dubbo_node_config_util:gen_provider_info(ProviderConfigInfo),
    ?assert(is_binary(ProvideNode)).


provider_parse_test() ->
    {ok, ProviderConfig} = dubbo_node_config_util:parse_provider_info(<<"dubbo%3A%2F%2F127.0.0.1%3A20880%2Forg.apache.dubbo.test.interface%3Finterface=org.apache.dubbo.test.interface&application=defaultApp&anyhost=true&dubbo=2.5.3&executes=10&methods=method1&side=provider&timestamp=1556095933071">>),
    ?assertEqual(ProviderConfig#provider_config.protocol, dubbo),
    ?assertEqual(ProviderConfig#provider_config.host, "127.0.0.1"),
    ?assertEqual(ProviderConfig#provider_config.port, 20880),
    ?assertEqual(ProviderConfig#provider_config.interface, <<"org.apache.dubbo.test.interface">>),
    ?assert(true).

