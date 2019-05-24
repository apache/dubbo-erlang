% ---------------------------------------------------------------------------
%   Copyright (C) 2008 0x6e6562
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
% ---------------------------------------------------------------------------

-module(dubbo_type_decoding).
-include("hessian.hrl").

%% The encoding state contains all of the statically known tuple types.
%% When a tuple is to be decoded at run-time, a lookup is performed against
%% the hash value that the sender passes in.
%% This must resolve to some type definition,
%% otherwise no instance of the decoded type can be created.
-record(decoding_state, {type_pool = dict:new(), reference_pool = dict:new(), obj_define_no = 0}).

-export([init/0, init/1]).
-export([lookup_reference/2]).
-export([enlist/1, enlist/2]).
-export([hash_lookup/2, hash_store/2, hash_store/1, store_typepool/1]).
-export([visit/2]).
-export([resolve_native_type/2, resolve_native_type/1]).
-export([camel_case_to_erlang/1]).
-export([build_foreign_view/3]).
-export([project_native_view/3]).

-define(TYPEPOOL_TABLE, type_pool).
-define(REFERENCEPOOL_TABLE, reference_pool).

camel_case_to_erlang(String) when is_binary(String) ->
    AsList = binary_to_list(String),
    AsErlang = lists:foldl(fun decamelize/2, [], AsList),
    list_to_atom(AsErlang).

decamelize(Element, Acc) when Element >= $A, Element =< $Z ->
    lists:append(Acc, [$_, (Element + 16#20)]);
decamelize(Element, Acc) -> lists:append(Acc, [Element]).

%% Facility to register a particular type to the pool of known types.
%% Adds the type to the pool of known types if it doesn't already exist.
enlist(TypeDef) -> enlist(TypeDef, init()).
enlist(TypeDef = #type_def{foreign_type = Key},
    State = #decoding_state{type_pool = OldPool}) ->
    ets:insert(?TYPEPOOL_TABLE, {Key, TypeDef}),
    State.

store_typepool(TypeDef = #type_def{foreign_type = Key}) ->
    ets:insert(?TYPEPOOL_TABLE, {Key, TypeDef}),
    TypeDef.



build_foreign_view(ForeignType, FieldNames, State) ->
    #type_def{native_type = Native} = resolve_native_type(ForeignType, State),
    ForeignView = [camel_case_to_erlang(Fieldname) || Fieldname <- FieldNames],
    DefineNo = State#decoding_state.obj_define_no,
    {#type_def{
        defineNo = State#decoding_state.obj_define_no,
        native_type = Native,
        foreign_type = ForeignType,
        fieldnames = ForeignView}, State#decoding_state{obj_define_no = DefineNo}}.

% Projects the native view over tuples that in foreign order
project_native_view(ForeignView, ForeignData,
    #type_def{native_type = NativeType, fieldnames = NativeView}) ->
    AsDict = dict:from_list(lists:zip(ForeignView, ForeignData)),
    NativeData = [dict:fetch(Key, AsDict) || Key <- NativeView],
    list_to_tuple([NativeType] ++ NativeData).

resolve_native_type(ForeignType, #decoding_state{type_pool = Pool}) ->
    case ets:lookup(?TYPEPOOL_TABLE, ForeignType) of
        [] ->
            throw({cannot_resolve_type, ForeignType});
        [{ForeignType, TypeDef}] ->
            TypeDef
    end.
resolve_native_type(ForeignType) ->
    case ets:lookup(?TYPEPOOL_TABLE, ForeignType) of
        [] ->
            undefined;
        [{ForeignType, TypeDef}] ->
            TypeDef
    end.
%%    case dict:fetch(ForeignType,Pool) of
%%        error ->
%%            throw({cannot_resolve_type,ForeignType});
%%        TypeDef ->
%%            TypeDef
%%    end.

%% Creates a reference for a type def that is unique within the
%% current invocation context
visit(TypeDef, State = #decoding_state{reference_pool = OldPool}) ->
    Size = ets:info(?REFERENCEPOOL_TABLE, size),
    ets:insert(?REFERENCEPOOL_TABLE, {Size, TypeDef#type_def{defineNo = Size}}),
    State.
%%    Size = dict:size(OldPool),
%%    NewPool = dict:store(Size, TypeDef, OldPool),
%%    State#decoding_state{reference_pool = NewPool}.

%% Resolves a type def based on a reference that must be have set
%% with the current invocation context
lookup_reference(Ref, #decoding_state{reference_pool = Pool}) ->
    case ets:lookup(?REFERENCEPOOL_TABLE, Ref) of
        [] ->
            throw("Lookup of unknown reference");
        [{Ref, TypeDef}] ->
            TypeDef
    end.
%?LOG(Pool),
%?LOG(Ref),
%%    case dict:fetch(Ref,Pool) of
%%        error ->
%%%%            ?ERROR(Ref, Pool),
%%            throw("Lookup of unknown reference");
%%        TypeDef ->
%%            TypeDef
%%    end.

%% Does what it says on the tin.
hash_lookup(Hash, _State) ->
%%    init(false),
    case ets:lookup(?REFERENCEPOOL_TABLE, Hash) of
        [] ->
            {not_found, Hash};
        [{Hash, TypeDef}] ->
            TypeDef
    end.

%% Does what it says on the tin.
hash_store(#type_def{defineNo = Hash} = TypeDef, State) ->
%%    init(false),
    case Hash of
        -1 ->
            Size = ets:info(?REFERENCEPOOL_TABLE, size),
            ets:insert(?REFERENCEPOOL_TABLE, {Size, TypeDef#type_def{defineNo = Size}});
        _ ->
            ets:insert(?REFERENCEPOOL_TABLE, {Hash, TypeDef})
    end,
    State.
hash_store(#type_def{defineNo = Hash} = TypeDef) ->
%%    init(false),
    NewTypeDef = case Hash of
                     -1 ->
                         Size = ets:info(?REFERENCEPOOL_TABLE, size),
                         TypeDef2 = TypeDef#type_def{defineNo = Size},
                         TypeDef2;
                     _ ->
                         TypeDef
                 end,
    ets:insert(?REFERENCEPOOL_TABLE, {NewTypeDef#type_def.defineNo, NewTypeDef}),
    NewTypeDef.

init() -> init(false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Non-API functions


%% This creates a new ETS table for the types that have been received by
%% this instance of Hessian
init(Delete) when is_boolean(Delete) ->
%%    case ets:info(hashes) of
%%        undefined ->
%%            io:format("init decoding table pid ~p~n",[self()]),
%%            ets:new(hashes,[public,named_table]); %% public
%%        EtsInfo ->
%%%%            io:format("type decoding etsinfo ~p~n",[EtsInfo]),
%%            if
%%                Delete == true ->
%%                    ets:delete(hashes),
%%                    ets:new(hashes,[public,named_table]);
%%                true ->
%%                    ok
%%            end
%%    end,
    case ets:info(?TYPEPOOL_TABLE) of
        undefined ->
            io:format("init decoding type_pool table pid ~p~n", [self()]),
            ets:new(?TYPEPOOL_TABLE, [public, named_table]); %% public
        _ ->
%%            io:format("type decoding etsinfo ~p~n",[EtsInfo]),
            if
                Delete == true ->
                    ets:delete(?TYPEPOOL_TABLE),
                    ets:new(?TYPEPOOL_TABLE, [public, named_table]);
                true ->
                    ok
            end
    end,
    case ets:info(?REFERENCEPOOL_TABLE) of
        undefined ->
            io:format("init decoding REFERENCEPOOL_TABLE table pid ~p~n", [self()]),
            ets:new(?REFERENCEPOOL_TABLE, [public, named_table]); %% public
        _ ->
            if
                Delete == true ->
                    ets:delete(?REFERENCEPOOL_TABLE),
                    ets:new(?REFERENCEPOOL_TABLE, [public, named_table]);
                true ->
                    ok
            end
    end,
    #decoding_state{}.