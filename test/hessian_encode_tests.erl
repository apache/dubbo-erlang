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
-module(hessian_encode_tests).
-include("hessian.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([object_test/0]).

-record(de_TestReq, {name, nick, age}).
-record(de_reg2, {reqinfo, age}).

object_test() ->
    ForeignTypeA = <<"com.ifcoder.demo.bean.UserInfoRequest">>,
    TypeDefA = #type_def{foreign_type = ForeignTypeA,
        native_type = de_TestReq,
        fieldnames = record_info(fields, de_TestReq)},
    EncodingState0 = dubbo_type_encoding:enlist(TypeDefA),
    RequestArg0 = #de_TestReq{name = <<"nameinfo">>, nick = <<"nickname">>, age = 10},

    {Bin, State0} = cotton_hessian:encode(RequestArg0, EncodingState0),

    dubbo_type_register:init(),
    dubbo_type_transfer:pre_process_typedef(de_TestReq, <<"com.ifcoder.demo.bean.UserInfoRequest">>, record_info(fields, de_TestReq)),
    {<<>>, Data, State2} = cotton_hessian:decode(Bin, cotton_hessian:init()),
    DecodeResult = dubbo_type_transfer:java_to_native(Data, State2),
    ?assert(is_record(DecodeResult, de_TestReq)),
    ?assertEqual(DecodeResult#de_TestReq.name, <<"nameinfo">>),
    ?assertEqual(DecodeResult#de_TestReq.nick, <<"nickname">>),
    ?assertEqual(DecodeResult#de_TestReq.age, 10),
    ?debugFmt("get decode info ~p", [DecodeResult]),
    ok.
