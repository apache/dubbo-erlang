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
-module(dubbo_consumer_pool_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").

update_readonly_test() ->
    dubbo_consumer_pool:start_link(),
    InterfaceName= <<"testinterfacename">>,
    HostFalg= <<"127.0.0.1/20880">>,
    ConnectionList = [
        #connection_info{connection_id=1,pid= testpid,weight = 30,host_flag = HostFalg},
        #connection_info{connection_id=2,pid= testpid2,weight = 30,host_flag = HostFalg}
    ],
    dubbo_consumer_pool:update_connection_info(InterfaceName,HostFalg,ConnectionList,true),
    {ok,Size} = dubbo_consumer_pool:update_connection_readonly(testpid,false),
    ?assertEqual(1,Size).
