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
