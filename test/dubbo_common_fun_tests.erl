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
-module(dubbo_common_fun_tests).
-include_lib("eunit/include/eunit.hrl").

request_gen_test() ->
    dubbo_id_generator:init([]),
    Id = dubbo_id_generator:gen_id(),
    ?assert(is_integer(Id)).

string_join_test() ->
    Result1 = dubbo_lists_util:join([<<"a">>, <<"b">>], <<",">>),
    ?assertEqual(Result1, <<"a,b">>),

    Result2 = dubbo_lists_util:join([], <<",">>),
    ?assertEqual(Result2, <<"">>),

    Result3 = dubbo_lists_util:join([<<"a">>, "b", ttt], <<",">>),
    ?assertEqual(Result3, <<"a,b">>),
    ok.

list_dup_test() ->
    R = dubbo_lists_util:del_duplicate([a, b, a]),
    ?assertEqual(length(R), 2).