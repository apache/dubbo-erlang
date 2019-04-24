%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 11:28 PM
%%%-------------------------------------------------------------------
-module(dubbo_common_fun_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").

request_gen_test() ->
    de_id_count:init([]),
    Id = de_id_count:gen_id(),
    ?assert(is_integer(Id)).

string_join_test()->
    Result1 = lists_util:join([<<"a">>,<<"b">>],<<",">>),
    ?assertEqual(Result1,<<"a,b">>),

    Result2 = lists_util:join([],<<",">>),
    ?assertEqual(Result2,<<"">>),

    Result3 = lists_util:join([<<"a">>,"b",ttt],<<",">>),
    ?assertEqual(Result3,<<"a,b">>),
    ok.

list_dup_test()->
    R = lists_util:del_duplicate([a,b,a]),
    ?assertEqual(length(R),2).