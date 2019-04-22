%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十月 2016 下午8:28
%%%-------------------------------------------------------------------
-module(de_type_transfer).
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

    case hessianDecode2:get_deftype(Data#object.typeRef,State) of
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
%%    case hessianDecode2:get_deftype(Data#list.refNo,State) of
%%        #type_def{fieldnames = ObjectFields,foreign_type = ForeignType } ->
%%            case get_deftype(ForeignType) of
%%                false->
%%                    error;
%%                #type_def{fieldnames = NativeFields,native_type = NativeTupeName}->
%%                    logger:debug("test ForeignType ~p NativeTupeName ~p",[ForeignType,NativeTupeName]),
%%%%                    AsDict = dict:from_list(lists:zip(ObjectFields,ForeignDataNew)),
%%%%                    NativeData = [dict:fetch(atom_to_binary(Key,utf8),AsDict) || Key <- NativeFields],
%%%%                    list_to_tuple( [NativeTupeName] ++ NativeData)
%%                    ForeignDataNew
%%            end;
%%        Info ->
%%            logger:warning("java_to_native list error:~p",[Info]),
%%            error
%%    end;
java_to_native(Data,_)->
    logger:debug("java_to_native unkonw type ~p",[Data]),
    Data.

%%get_deftype([Item |DefTypeList],ForeignType)->
%%    if
%%        Item#type_def.foreign_type == ForeignType -> Item;
%%        true ->
%%            get_deftype(DefTypeList,ForeignType)
%%    end;
%%get_deftype([],_ForeignType)->
%%    false.

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
    logger:debug("pre_process_typedef ~p,~p",[NativeType,ForeignType]).
%%    case type_decoding:resolve_native_type(ForeignType) of
%%        undefined ->
%%%%            Type = #type_def{native_type = NativeType, foreign_type = ForeignType, fieldnames = record_info(fields,NativeType)},
%%            Type = #type_def{native_type = NativeType,foreign_type = ForeignType,fieldnames = FieldsNames},
%%            Type2=type_decoding:hash_store(Type),
%%
%%            logger:debug("pre_process_typedef ~p,~p",[NativeType,ForeignType]);
%%            type_decoding:store_typepool(Type2);
%%        _->
%%            ok
%%    end.