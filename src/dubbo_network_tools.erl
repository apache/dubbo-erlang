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
-module(dubbo_network_tools).

%% API
-export([local_ip_v4/0, local_ipv4/0, local_ipv4_binary/0]).


local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127, 0, 0, 1}
    ]).

local_ipv4_binary() ->
    {I1, I2, I3, I4} = local_ip_v4(),
    list_to_binary(io_lib:format("~p.~p.~p.~p", [I1, I2, I3, I4])).
local_ipv4() ->
    {I1, I2, I3, I4} = local_ip_v4(),
    lists:flatten(io_lib:format("~p.~p.~p.~p", [I1, I2, I3, I4])).
