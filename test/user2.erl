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
-module(user2).

-include_lib("dubboerl/include/dubbo.hrl").
-include_lib("dubboerl/include/hessian.hrl").

-define(CURRENT_CLASS_NAME, <<"com.ifcoder.demo.facade.User"/utf8>>).
-define(CURRENT_CLASS_VERSION, <<"0.0.0"/utf8>>).

-include("dubbo_service.hrl").


-export([test/0]).

%% API
-export([
    getUserInfo/1,
    getUserInfo/2,
    genUserId/0,
    genUserId/1,
    queryUserInfo/1,
    queryUserInfo/2,
    queryUserList/1,
    queryUserList/2]).

-export([get_method_999_list/0]).

%% behaviour
-callback getUserInfo(Arg0 :: list()) -> #userInfo{}.
-callback genUserId() -> list().
-callback queryUserInfo(Arg0 :: #userInfoRequest{}) -> #userInfo{}.
-callback queryUserList(Arg0 :: list()) -> #userRes{}.

get_method_999_list() ->
    [
        getUserInfo,
        genUserId,
        queryUserInfo,
        queryUserList].



-spec getUserInfo(Arg0 :: list()) ->
    {ok, reference()}|
    {ok, reference(), Data :: #userInfo{}, RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
getUserInfo(Arg0) ->
    getUserInfo(Arg0, #{}).

getUserInfo(Arg0, RequestOption) ->

    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"getUserInfo">>,
        parameterDesc = <<"Ljava/lang/String;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"java.lang.String">>,
                native_type = string,
                fieldnames = []}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">>, ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME, Request, RequestOption).


-spec genUserId() ->
    {ok, reference()}|
    {ok, reference(), Data :: list(), RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
genUserId() ->
    genUserId(#{}).

genUserId(RequestOption) ->

    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"genUserId">>,
        parameterDesc = <<""/utf8>>,
        parameterTypes = [

        ],
        parameters = [

        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">>, ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME, Request, RequestOption).


-spec queryUserInfo(Arg0 :: #userInfoRequest{}) ->
    {ok, reference()}|
    {ok, reference(), Data :: #userInfo{}, RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
queryUserInfo(Arg0) ->
    queryUserInfo(Arg0, #{}).

queryUserInfo(Arg0, RequestOption) ->

    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"queryUserInfo">>,
        parameterDesc = <<"Lcom/ifcoder/demo/bean/UserInfoRequest;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"com.ifcoder.demo.bean.UserInfoRequest">>,
                native_type = userInfoRequest,
                fieldnames = record_info(fields, userInfoRequest)}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">>, ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME, Request, RequestOption).


-spec queryUserList(Arg0 :: list()) ->
    {ok, reference()}|
    {ok, reference(), Data :: #userRes{}, RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
queryUserList(Arg0) ->
    queryUserList(Arg0, #{}).

queryUserList(Arg0, RequestOption) ->

    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"queryUserList">>,
        parameterDesc = <<"Ljava/lang/String;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"java.lang.String">>,
                native_type = string,
                fieldnames = []}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">>, ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME, Request, RequestOption).


test() ->
    queryUserInfo(#userInfoRequest{username = "name", requestId = "111"}, #{sync=> true}).