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
-module(dubbo_time_util).

-include_lib("kernel/include/file.hrl").


-export([
    get_cur_time/0, get_cur_time/1,
    format_time_to_str/1,
    timestamp/0, timestamp_ms/0,
    timestamp_to_datetime/1,
    timestamp_to_local_datetime/1,
    get_cur_date/0,
    datetime_to_timestamp/1]).


get_cur_time() ->
    {{Year, Month, Day}, {Hour, Min, Second}} = calendar:now_to_local_time(os:timestamp()),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Min, Second]).

get_cur_date() ->
    {{Year, Month, Day}, {_Hour, _Min, _Second}} = calendar:now_to_local_time(os:timestamp()),
    io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]).

get_cur_time({{Year, Month, Day}, {Hour, Min, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Min, Second]).

format_time_to_str({{Year, Month, Day}, {Hour, Min, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Min, Second]).

timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
timestamp_ms() ->
    {M, S, W} = os:timestamp(),
    M * 1000000000 + S * 1000 + (W div 1000).

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).
timestamp_to_local_datetime(Timestamp) ->
    Date = calendar:gregorian_seconds_to_datetime(Timestamp +
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
    calendar:universal_time_to_local_time(Date).

datetime_to_timestamp(Date) ->
    [{D, T}] = calendar:local_time_to_universal_time_dst(Date),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    (S - S1).
