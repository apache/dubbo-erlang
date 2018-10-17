-ifndef(HESSIAN_H).
-define(HESSIAN_H,1).

-define(M, 2).
-define(m, 0).

-define(CHUNK_SIZE, 1024).

-define(MegaSeconds, 1000000000).
-define(Seconds, 1000).
-define(MicroSeconds, 1000).
-define(UnixEpoch, 62167219200).

%% Equivalents: type_def and class
-record(type_def,{defineNo=-1,native_type, foreign_type, fieldnames}).
-record(class, {typeNo=-1, encoded=false, name=[], fields=[]}).

-record(list, {refNo=-1, len=-1, type=untyped, values=[]}).
-record(map, {refNo=-1, type=untyped, dict=dict:new()}).
-record(object, {refNo=-1, typeRef=-1, class=auto, values=[]}).

-record(set, {ref=-1, value=[]}).

-ifdef(DEBUG).
% Application Logging
-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
-define(ERROR(Error,Reason), io:format("Error @ ~p:~p: ~p (Reason: ~p)~n", [?MODULE, ?LINE,Error,Reason])).
% Protocol Logging
-define(START(Msg,Value), io:format("~s -> ~p~n", [Msg,Value])).
-define(METHOD(Value), io:format("\tMethod -> ~p~n", [ binary_to_list(Value) ] )).
-define(VALUE(Value), io:format("\t\tValue -> ~p~n", [ Value ] )).
-define(TYPEDEF(Value), io:format("\t\tTypeDef -> ~p~n", [ Value ] )).
-define(INSTANCE(Value), io:format("\t\tInstance -> ~p~n", [ Value ] )).
-else.
% Application Logging
-define(LOG(Msg), true).
-define(ERROR(Msg), true).
% Protocol Logging
-define(START(Msg,Value), true).
-define(METHOD(Value), true).
-define(VALUE(Value), true).
-define(TYPEDEF(Value), true).
-define(INSTANCE(Value), true).
-endif.

-endif.