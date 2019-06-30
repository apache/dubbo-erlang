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
-include("hessian.hrl").

-define(DUBBO_VERSION, <<"2.6.0">>).
-define(DUBBO_MEGIC, -9541). %% new version 16#dabb
-define(DUBBO_MEGIC_HIGH, 16#da). %% new version 16#da
-define(DUBBO_MEGIC_LOW, 16#bb). %% new version 16#bb

-define(DUBBO_DEFAULT_PORT, 20880).

%% 序列化类型
-define(SERIALIZATION_HESSIAN, 2).
-define(SERIALIZATION_FASTJSON, 6).
-define(SERIALIZATION_KRYO, 8).

-define(RESPONSE_WITH_EXCEPTION, 0).
-define(RESPONSE_VALUE, 1).
-define(RESPONSE_NULL_VALUE, 2).


-define(RESPONSE_STATE_OK, 20).

-define(REQUEST_TIME_OUT, 5000).

-define(LINE_SEPERATOR, <<"\n"/utf8>>).

-record(dubbo_request, {
    serialize_type = 2 :: integer(),
    is_event = true :: boolean(),
    is_twoway = false :: boolean(),
    data :: null|dubbo_rpc_invocation,
    mid :: integer(),
    mversion :: binary(),
    error_msg :: binary(),
    state :: byte(),
    decode_state
}).

-record(dubbo_response, {
    serialize_type = 2 :: integer(),
    is_event = true :: boolean(),
    is_twoway = false :: boolean(),
    data :: null|dubbo_rpc_invocation,
    mid :: integer(),
    mversion :: string(),
    error_msg :: string(),
    state :: byte(),
    decode_state
}).


-record(reference_config,{
    interface,
    application = <<"NoName">> :: binary(),
    category = <<"consumers">> :: binary(),
    check = false :: boolean(),
    default_timeout = 500 :: integer(),
    dubbo_version = <<"2.5.3">> :: binary(),
    methods = [] :: list(),
    revision = <<"">> :: binary(),
    side = <<"consumers">> :: binary(),
    sync = false ::boolean()

}).

-record(dubbo_rpc_invocation, {
    serialVersionUID = -4355285085441097045,
    className :: binary(),
    classVersion :: binary(),
    methodName :: binary(),
    parameterDesc :: binary(),
    parameterTypes = [] :: [#type_def{}],
    parameters = [] :: [term()],
    attachments = [] :: [term()],

    call_ref :: atom(),
    reference_ops :: #reference_config{},
    loadbalance :: atom(),
    source_pid :: pid(),
    transport_pid :: pid()

}).

-record(consumer_config, {
    interface,
    application = <<"NoName">> :: binary(),
    category = <<"consumers">> :: binary(),
    check = false :: boolean(),
    default_timeout = 500 :: integer(),
    dubbo_version = <<"2.5.3">> :: binary(),
    methods = [] :: list(),
    revision = <<"">> :: binary(),
    side = <<"consumers">> :: binary()
}).

-record(provider_config, {
    protocol,
    host,
    port,
    interface,
    anyhost = true,
    executes = 1,
    application,
    dubbo = <<"2.5.3">>,
    methods = [],
    side = <<"provider">>,
    impl_handle
}).

-record(invoker,{
    url,
    handler
}).


-record(interface_info, {interface, loadbalance, protocol}).

-record(interface_list, {interface, pid, connection_info}).
-record(connection_info, {host_flag, pid, weight, readonly = false}).

-type dubbo_request() :: #dubbo_request{}.
-type dubbo_response() :: #dubbo_response{}.
-type invocation():: #dubbo_rpc_invocation{}.

%% @doc invoke return info
-type invoke_result() :: {ok, reference()}| {ok, reference(), Data :: any(), RpcContent :: list()}| {error, Reason :: timeout|no_provider|any()}.