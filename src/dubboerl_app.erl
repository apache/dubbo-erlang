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
-module(dubboerl_app).

-behaviour(application).

-include("dubboerl.hrl").
%% Application callbacks
-export([start/2, stop/1, env_init/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    logger:info("[START] dubbo framework server start"),
    case dubboerl_sup:start_link() of
        {ok, Pid} ->
            init_default_hooks(),
            {ok, Pid};
        Result ->
            Result
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
init_default_hooks() ->
    dubbo_extension:register(protocol, dubbo_protocol_dubbo, 10),
    dubbo_extension:register(protocol_wapper, dubbo_protocol_registry, 10),
    dubbo_extension:register(filter, application:get_env(dubboerl, cluster, dubbo_cluster_failfast), 1),
    ok.
env_init() ->
    ets:new(?PROVIDER_IMPL_TABLE, [public, named_table]),
    ets:new(?SERVICE_EXPORT_TABLE, [public, named_table]),
    dubbo_traffic_control:init(),
    dubbo_type_register:init(),
    register_type_list().
%%    type_decoding:init().


register_type_list() ->
    List = dubbo_java_type_defined:get_list(),
    lists:map(
        fun({NativeType, ForeignType, Fields}) ->
            dubbo_type_transfer:pre_process_typedef(NativeType, ForeignType, Fields)
        end, List),
    ok.