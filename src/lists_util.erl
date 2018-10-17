%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2017 6:09 PM
%%%-------------------------------------------------------------------
-module(lists_util).
-author("dlive").

%% API
-export([join/2,del_duplicate/1]).

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
                    [X]++ List2
            end
        end, [], List).
