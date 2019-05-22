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
-module(dubbo_traffic_control_tests).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    dubbo_traffic_control:init(),
    dubbo_traffic_control:init(),
    ?assert(true).

goon_test() ->
    dubbo_traffic_control:init(),
    ?assertEqual(dubbo_traffic_control:check_goon(key1, 2), ok),
    ?assertEqual(dubbo_traffic_control:check_goon(key1, 2), ok),
    ?assertEqual(dubbo_traffic_control:check_goon(key1, 2), full),
    ?assertEqual(dubbo_traffic_control:check_goon(key1, 2), full),
    ?assertEqual(dubbo_traffic_control:decr_count(key1), ok),
    ok.
