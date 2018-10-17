%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2018 2:06 PM
%%%-------------------------------------------------------------------
-module(type_register).
-author("dlive").
%% API
-export([init/0,regiest_foreign_native/1,lookup_foreign_type/1,lookup_native_type/1]).
-include("hessian.hrl").
-define(FOREIGN_NATIVE_TABLE,foreign_native_table).
-define(NATIVE_FOREIGN_TABLE,native_foreign_table).

init()->
    case ets:info(?FOREIGN_NATIVE_TABLE) of
        undefined ->
            io:format("init decoding foreign_native_table table pid ~p~n",[self()]),
            ets:new(?FOREIGN_NATIVE_TABLE,[public,named_table]); %% public
        _ ->
            ets:delete(?FOREIGN_NATIVE_TABLE),
            ets:new(?FOREIGN_NATIVE_TABLE,[public,named_table])
    end,
    case ets:info(?NATIVE_FOREIGN_TABLE) of
        undefined ->
            io:format("init decoding foreign_native_table table pid ~p~n",[self()]),
            ets:new(?NATIVE_FOREIGN_TABLE,[public,named_table]); %% public
        _ ->
            ets:delete(?NATIVE_FOREIGN_TABLE),
            ets:new(?NATIVE_FOREIGN_TABLE,[public,named_table])
    end,
    ok.


regiest_foreign_native(TypeDef)->
    lager:debug("regiest foreign info ~p",[TypeDef]),
    ets:insert(?FOREIGN_NATIVE_TABLE,{TypeDef#type_def.foreign_type,TypeDef}),
    ets:insert(?NATIVE_FOREIGN_TABLE,{TypeDef#type_def.native_type,TypeDef}).


lookup_foreign_type(ForeignType)->
    case ets:lookup(?FOREIGN_NATIVE_TABLE,ForeignType) of
        []->
            undefined;
        [{_,TypeDef}] ->
            TypeDef
    end.

lookup_native_type(NativeType)->
    case ets:lookup(?NATIVE_FOREIGN_TABLE,NativeType) of
        []->
            undefined;
        [{_,TypeDef}] ->
            TypeDef
    end.