%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Mar 2018 11:06 PM
%%%-------------------------------------------------------------------
-module(java_type_defined).
-author("dlive").

-include("java_type.hrl").
%% API
-export([get_list/0]).


get_list()->
    [
        {null_pointer_exception,<<"java.lang.NullPointerException">>,record_info(fields,null_pointer_exception)},
        {stack_stack_trace_element,<<"java.lang.StackTraceElement">>,record_info(fields,stack_stack_trace_element)}
    ].