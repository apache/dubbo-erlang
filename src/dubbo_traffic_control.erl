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
-module(dubbo_traffic_control).
-include("dubboerl.hrl").
%% API
-export([init/0, check_goon/2, decr_count/1]).


init() ->
    case ets:info(?TRAFFIC_CONTROL) of
        undefined ->
            io:format("init decoding TRAFFIC_CONTROL table pid ~p~n", [self()]),
            ets:new(?TRAFFIC_CONTROL, [public, named_table, {write_concurrency, true}]); %% public
        _ ->
            ets:delete(?TRAFFIC_CONTROL),
            ets:new(?TRAFFIC_CONTROL, [public, named_table, {write_concurrency, true}])
    end,
    ok.


check_goon(Key, Max) ->
    try ets:update_counter(?TRAFFIC_CONTROL, Key, 1) of
        Value when Value > Max ->
            ets:update_counter(?TRAFFIC_CONTROL, Key, -1),
            full;
        _V ->
            ok
    catch
        _T:_R ->
            ets:insert(?TRAFFIC_CONTROL, {Key, 1}),
            ok
    end.

decr_count(Key) ->
    try ets:update_counter(?TRAFFIC_CONTROL, Key, -1) of
        _V ->
            ok
    catch
        _T:_R ->
            ets:insert(?TRAFFIC_CONTROL, {Key, 0}),
            ok
    end.