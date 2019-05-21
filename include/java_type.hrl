%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Mar 2018 11:10 PM
%%%-------------------------------------------------------------------
-record(null_pointer_exception,{detailMessage,cause,stackTrace,suppressedExceptions}).

-record(stack_stack_trace_element,{declaringClass,methodName,fileName,lineNumber}).