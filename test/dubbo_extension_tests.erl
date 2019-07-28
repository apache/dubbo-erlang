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
-module(dubbo_extension_tests).


-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").

invoker_test()->
    {ok,_Pid} = dubbo_extension:start_link(),

    ok = dubbo_extension:register(filter, dubbo_filter_test1, 100),
    Invocation = #dubbo_rpc_invocation{},
    Ref = make_ref(),
    {ok,Ref} = dubbo_extension:invoke(filter, invoke, [Invocation], {ok, Ref}, []),
    ok.

%%on_response_test() ->



