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
-module(dubbo_adapter_tests).
-include("dubbo.hrl").
-include_lib("eunit/include/eunit.hrl").

reference_test() ->
    dubbo_id_generator:start_link(),
    Invocation = #dubbo_rpc_invocation{
        className = <<"testname">>,
        classVersion = <<"testversion">>,
        methodName = <<"getUserInfo">>,
        parameterDesc = <<"Ljava/lang/String;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"java.lang.String">>,
                native_type = string,
                fieldnames = []}
        ],
        parameters = [
            <<"test">>
        ],
        attachments = [
            {<<"path">>, <<"testname">>},
            {<<"interface">>, <<"testname">>}
        ]
    },
    Request = dubbo_adapter:reference(Invocation),
    ?assert(is_record(Request, dubbo_request)).
