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
-module(dubbo_service_type_list).

%% API
-export([register_type_list/0, get_list/0]).

-include("dubbo_service.hrl").

get_list() ->
    [
        {userInfoRequest, <<"com.ifcoder.demo.bean.UserInfoRequest">>, record_info(fields, userInfoRequest)},
        {userInfo, <<"com.ifcoder.demo.bean.UserInfo">>, record_info(fields, userInfo)},
        {userRes, <<"com.ifcoder.demo.bean.UserRes">>, record_info(fields, userRes)}].

register_type_list() ->
    ok.