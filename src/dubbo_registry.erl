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
-callback register(Url::binary())-> term().
-callback subscribe(SubcribeUrl::binary(),NotifyFun::function())->ok.

%% API
-export([setup_register/1,register/2]).

-spec(setup_register(UrlInfo :: map()) -> {ok, RegistryProcessName :: atom()}|{error, term()}).
setup_register(UrlInfo) ->
    RegistryModuleName = get_registry_module(UrlInfo),
    case whereis(RegistryModuleName) of
        undefined ->
            apply(RegistryModuleName, start, [UrlInfo]),
            {ok, RegistryModuleName};
        _ ->
            {ok, RegistryModuleName}
    end.

register(RegistryName,Url) ->
    logger:info("call ~p register url ~p",[RegistryName,Url]),
    Result = apply(RegistryName,register,[Url]),
    Result.


get_registry_module(Info) ->
    RegistryName = Info#dubbo_url.scheme,
    FullName = << <<"dubbo_registry_">>, RegistryName/binary>>,
    binary_to_existing_atom(FullName).