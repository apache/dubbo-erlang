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
-module(dubbo_lists_util).
%% API
-export([join/2, del_duplicate/1]).

-spec(join(List :: list(), Separator :: binary()) -> binary()).
join(List, _Separator) when length(List) == 0 ->
    <<"">>;
join(List, Separator) ->
    [First | Rst] = List,
    Acc2 = lists:foldl(fun(Item, Acc) ->
        if
            is_binary(Item) ->
                <<Acc/binary, Separator/binary, Item/binary>>;
            is_list(Item) ->
                Item2 = list_to_binary(Item),
                <<Acc/binary, Separator/binary, Item2/binary>>;
            true ->
                Acc
        end
                       end, First, Rst),
    Acc2.


del_duplicate(List) ->
    lists:foldl(
        fun(X, List2) ->
            case lists:member(X, List2) of
                true ->
                    List2;
                _ ->
                    [X] ++ List2
            end
        end, [], List).
