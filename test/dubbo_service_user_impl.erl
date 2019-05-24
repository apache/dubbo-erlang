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
-module(dubbo_service_user_impl).


-behaviour(userOperator).

-include_lib("dubbo_sample_service.hrl").
-include_lib("dubboerl/include/hessian.hrl").
-include_lib("dubboerl/include/dubbo.hrl").
%% API
-export([getUserInfo/1, queryUserList/1, genUserId/0, queryUserInfo/1]).

genUserId() ->
    "newid".

getUserInfo(Args) ->
    io:format(user, "do invokeWs ~p", [Args]),
    #userInfo{userAge = 88, userName = "one", userId = "id123"}.

queryUserList(Args) ->
    User = #userInfo{userAge = 88, userName = "two", userId = "id123"},
    List = #list{len = 1, type = "java.util.ArrayList", values = [User]},

    Res = #userRes{
        userlist = List
    },
    Res.


queryUserInfo(Arg0) ->
    io:format(user, "do invoker queryUserInfo ~p", [Arg0]),
    #userInfo{userName = "uuname", userAge = 10, userId = "44"}.