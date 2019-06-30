%%------------------------------------------------------------------------------
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed with
%% this work for additional information regarding copyright ownership.
%% The ASF licenses this file to You under the Apache License, Version 2.0
%% (the "License"); you may not use this file except in compliance with
%% the License.  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------
-module(dubbo_type_transfer).
-include("hessian.hrl").
-include("dubbo.hrl").

%% API
-export([java_to_native/2, pre_process_typedef/3, response_to_native/1, classobj_to_native/2, jsonobj_to_native/3]).


response_to_native(Response) ->
    java_to_native(Response#dubbo_response.data, Response#dubbo_response.decode_state).


classobj_to_native(Data, DecodeState) ->
    java_to_native(Data, DecodeState).

java_to_native(#object{values = ForeignData} = Data, State) ->
    ForeignDataNew = [java_to_native(ValueItem, State) || ValueItem <- ForeignData],

    case cotton_hessian:get_deftype(Data#object.typeRef, State) of
        #type_def{fieldnames = ObjectFields, foreign_type = ForeignType} ->
            case get_deftype(ForeignType) of
                false ->
                    error;
                #type_def{fieldnames = NativeFields, native_type = NativeTupeName} ->
                    AsDict = dict:from_list(lists:zip(ObjectFields, ForeignDataNew)),
                    NativeData = [dict:fetch(atom_to_binary(Key, utf8), AsDict) || Key <- NativeFields],
                    list_to_tuple([NativeTupeName] ++ NativeData)
            end;
        Info ->
            logger:warning("java_to_native error:~p", [Info]),
            error
    end;
java_to_native(#list{values = ForeignData} = Data, State) ->
    ForeignDataNew = [java_to_native(ValueItem, State) || ValueItem <- ForeignData],
    ForeignDataNew;
java_to_native(Data, _) ->
    Data.

get_deftype(ForeignType) ->

    case dubbo_type_register:lookup_foreign_type(ForeignType) of
        undefined ->
            logger:debug("get deftype undefined ~p", [ForeignType]),
            false;
        #type_def{} = TypeDef ->
            logger:debug("get deftype success ~p", [ForeignType]),
            TypeDef;
        _ ->
            logger:debug("get deftype  undefined ~p", [ForeignType]),
            false
    end.

pre_process_typedef(NativeType, ForeignType, FieldsNames) ->
    Type = #type_def{native_type = NativeType, foreign_type = ForeignType, fieldnames = FieldsNames},
%%            Type2=type_decoding:hash_store(Type),
    dubbo_type_register:regiest_foreign_native(Type),
    logger:debug("pre_process_typedef ~p,~p", [NativeType, ForeignType]),
    ok.


jsonobj_to_native(Type, JsonObj, State) ->
    ClassName = java_desc_name_to_dot(Type),
    %% todo need recursion transfer
    case dubbo_type_register:lookup_foreign_type(ClassName) of
        undefined ->
            JsonObj;
        #type_def{fieldnames = Fields, native_type = NativeType} ->
            logger:debug("jsonobj_to_native ~p ~p ~p", [ClassName, Fields, JsonObj]),
            NativeData = [maps:get(atom_to_binary(Key, utf8), JsonObj, undefined) || Key <- Fields],
            list_to_tuple([NativeType] ++ NativeData)
    end.



java_desc_name_to_dot(DescName) ->
    case DescName of
        <<$L, ClassName/binary>> ->
            binary:replace(ClassName, <<"/">>, <<".">>, [global]);
        _ ->
            DescName
    end.

