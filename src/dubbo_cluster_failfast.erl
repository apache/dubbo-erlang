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
-module(dubbo_cluster_failfast).
-behaviour(dubbo_filter).

-include("dubbo.hrl").
%% API
-export([invoke/2, do_response/2]).


invoke(#dubbo_rpc_invocation{className = Interface, loadbalance = LoadBalance} = Invocation, Acc) ->
    case dubbo_provider_consumer_reg_table:select_connection(Invocation#dubbo_rpc_invocation.className) of
        {ok, List} ->
            Connection = loadbalance_select(LoadBalance, List),
            #connection_info{pid = Pid, host_flag = HostFlag} = Connection,
            {ok, Invocation#dubbo_rpc_invocation{transport_pid = Pid}, Acc};
%%            case dubbo_traffic_control:check_goon(HostFlag, 199) of
%%                ok ->
%%
%%%%                    Request2 = merge_attachments(Request, RpcContext), %% @todo need add rpc context to attachment
%%
%%%%                    {ok, RequestData} = dubbo_codec:encode_request(Request2),
%%%%                    Ref = get_ref(RequestState),
%%%%                    gen_server:cast(Pid, {send_request, Ref, Request2, RequestData, CallBackPid, RequestState}),
%%%%                    case is_sync(RequestState) of
%%%%                        true ->
%%%%                            sync_receive(Ref, get_timeout(RequestState));
%%%%                        false -> {ok, Ref}
%%%%                    end;
%%                full ->
%%                    {error, request_full}
%%            end;
        {error, none} ->
            logger:error("[INVOKE] ~p error Reason no_provider", [Interface]),
            {stop, no_provider}
    end.

loadbalance_select(LoadBalance, ConnectionList) ->
    Connection = LoadBalance:select(ConnectionList),
    Connection.

do_response(Invocation, Result) ->
    {ok, Invocation, Result}.
