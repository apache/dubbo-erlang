%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2019 23:57
%%%-------------------------------------------------------------------
-module(dubbo_heartbeat_tests).
-author("dlive").

-include_lib("eunit/include/eunit.hrl").

heartbeat1_test()->
  dubbo_id_generator:start_link(),
  {ok,Data} = dubbo_heartbeat:generate_request(undefined,false),
  ?assert(is_binary(Data)).

simple_test() ->
  ?assert(true).
