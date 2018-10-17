%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2018 4:49 PM
%%%-------------------------------------------------------------------
-module(de_codec_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").
-include("dubbo.hrl").

-record(databaseOperateRequest,{
    param}).


simple_test() ->
    ?assert(true).
