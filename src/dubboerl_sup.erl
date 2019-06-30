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
-module(dubboerl_sup).

-behaviour(supervisor).

-include("common.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    dubboerl_app:env_init(),
    %% @todo registry need move registry sup
    ZK = {dubbo_registry_zookeeper, {dubbo_registry_zookeeper, start_link, []}, transient, 5000, worker, [dubbo_registry_zookeeper]},

    ExtensionSer = {dubbo_extension, {dubbo_extension, start_link, []}, transient, 5000, worker, [dubbo_extension]},
    Id_count = {dubbo_id_generator, {dubbo_id_generator, start_link, []}, transient, 5000, worker, [dubbo_id_generator]},
    ProviderPoolSup = {dubbo_provider_worker_sup, {dubbo_provider_worker_sup, start_link, []}, transient, 5000, supervisor, [dubbo_provider_worker_sup]},
    ConsumerPoolSup = {dubbo_transport_pool_sup, {dubbo_transport_pool_sup, start_link, []}, transient, 5000, supervisor, [dubbo_transport_pool_sup]},
    ConsumerPool = {dubbo_provider_consumer_reg_table, {dubbo_provider_consumer_reg_table, start_link, []}, transient, 5000, worker, [dubbo_provider_consumer_reg_table]},
    ShutdownSer = {dubbo_shutdown, {dubbo_shutdown, start_link, []}, transient, 10000, worker, [dubbo_shutdown]},
%%    ListNew1 =
%%        case application:get_env(dubboerl, registry, false) of
%%            true ->
%%                [ZK];
%%            false ->
%%                []
%%        end,
    ListNew = [Id_count, ExtensionSer, ZK, ConsumerPool, ConsumerPoolSup, ProviderPoolSup, ShutdownSer],
    {ok, {{one_for_one, 60, 10}, ListNew}}.

%%====================================================================
%% Internal functions
%%====================================================================
