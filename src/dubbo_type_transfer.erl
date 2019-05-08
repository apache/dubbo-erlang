%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十月 2016 下午8:28
%%%-------------------------------------------------------------------
-module(dubbo_type_transfer).
-author("dlive").

-include("hessian.hrl").
-include("dubbo.hrl").

%% API
-export([java_to_native/2,pre_process_typedef/3,response_to_native/1,classobj_to_native/2]).


response_to_native(Response)->
    java_to_native(Response#dubbo_response.data,Response#dubbo_response.decode_state).


classobj_to_native(Data,DecodeState)->
    java_to_native(Data,DecodeState).

java_to_native(#object{values = ForeignData}=Data,State)->
    ForeignDataNew = [java_to_native(ValueItem,State) || ValueItem <-ForeignData ],

    case cotton_hessian:get_deftype(Data#object.typeRef,State) of
        #type_def{fieldnames = ObjectFields,foreign_type = ForeignType } ->
            case get_deftype(ForeignType) of
                false->
                    error;
                #type_def{fieldnames = NativeFields,native_type = NativeTupeName}->
                    AsDict = dict:from_list(lists:zip(ObjectFields,ForeignDataNew)),
                    NativeData = [dict:fetch(atom_to_binary(Key,utf8),AsDict) || Key <- NativeFields],
                    list_to_tuple( [NativeTupeName] ++ NativeData)
            end;
        Info ->
            logger:warning("java_to_native error:~p",[Info]),
            error
    end;
java_to_native(#list{values = ForeignData}=Data,State)->
    ForeignDataNew = [java_to_native(ValueItem,State) || ValueItem <-ForeignData ],
    ForeignDataNew;
java_to_native(Data,_)->
    logger:debug("java_to_native unkonw type ~p",[Data]),
    Data.

get_deftype(ForeignType)->

    case type_register:lookup_foreign_type(ForeignType) of
        undefined->
            logger:debug("get deftype undefined ~p",[ForeignType]),
            false;
        #type_def{}=TypeDef->
            logger:debug("get deftype success ~p",[ForeignType]),
            TypeDef;
        _->
            logger:debug("get deftype  undefined ~p",[ForeignType]),
            false
    end.

pre_process_typedef(NativeType,ForeignType,FieldsNames)->
    Type = #type_def{native_type = NativeType,foreign_type = ForeignType,fieldnames = FieldsNames},
%%            Type2=type_decoding:hash_store(Type),
    type_register:regiest_foreign_native(Type),
    logger:debug("pre_process_typedef ~p,~p",[NativeType,ForeignType]),
    ok.