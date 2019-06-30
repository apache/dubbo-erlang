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
-module(dubbo_registry).
-include("dubboerl.hrl").

-callback start(Url :: binary) -> ok.
-callback register(Url :: binary()) -> term().
-callback subscribe(SubcribeUrl :: binary(), NotifyFun :: function()) -> ok.

%% API
-export([setup_register/1, register/2, unregister/2, get_registry_host_port/0, get_registry_type/0, get_registry_module/1]).

-spec(setup_register(UrlInfo :: #dubbo_url{}) -> {ok, RegistryProcessName :: atom()}|{error, term()}).
setup_register(UrlInfo) ->
    RegistryModuleName = get_registry_module(UrlInfo),
    case whereis(RegistryModuleName) of
        undefined ->
            apply(RegistryModuleName, start, [UrlInfo]),
            {ok, RegistryModuleName};
        _ ->
            {ok, RegistryModuleName}
    end.

register(RegistryName, Url) ->
    logger:info("call ~p register url ~p", [RegistryName, Url]),
    Result = apply(RegistryName, register, [Url]),
    Result.
unregister(RegistryName, Url) ->
    logger:info("call ~p unregister url ~p", [RegistryName, Url]),
    Result = apply(RegistryName, unregister, [Url]),
    Result.

get_registry_module(Info) ->
    RegistryName = Info#dubbo_url.scheme,
    FullName = <<<<"dubbo_registry_">>/binary, RegistryName/binary>>,
    binary_to_existing_atom(FullName, latin1).



get_registry_host_port() ->
    %% @todo need adapter other registry
    RegistryList = application:get_env(dubboerl, zookeeper_list, [{"127.0.0.1", 2181}]),
    [Item | _] = RegistryList,
    Item.

get_registry_type() ->
    %%todo
    atom_to_binary(application:get_env(dubboerl, registry, zookeeper), utf8).