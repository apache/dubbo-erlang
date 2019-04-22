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

-module(type_encoding).

-include("hessian.hrl").

%% The encoding state contains all of the statically known tuple types.
%% When a tuple is to be encoded at run-time, a lookup is performed against
%% the type tag. This must resolve to some type definition,
%% otherwise no type information can be encoded into the output stream.
-record(encoding_state,{pool = dict:new(), count = -1}).

-export([init/0]).
-export([enlist/1,enlist/2]).
-export([visit/2]).

%% Facility to register a particular type to the pool of known types.
%% Adds the type to the pool of known types if it doesn't already exist.
init()->
    #encoding_state{}.

enlist(TypeDef) -> enlist(TypeDef,#encoding_state{}).
enlist(TypeDef = #type_def{native_type = Key},
         State = #encoding_state{pool = OldPool}) ->
    case dict:is_key(Key,OldPool) of
        true ->
            State;
        false ->
            NewPool = dict:store(Key, {-1, TypeDef}, OldPool),
            State#encoding_state{pool = NewPool}
    end.

%% Facility to record the fact that an instance of a type is about to be
%% encoded into the stream. This needs to decide whether the hash of the
%% type def has already been written or not.
%%
%% If not, a reference to this needs to be generated for future instances
%% and the hash value of the type def needs to be written out.
%%
%% If it already has been written out, it must be back-referenced.
visit(NativeType, State = #encoding_state{pool = Pool}) ->
    logger:debug("[encode] visit ~p",[NativeType]),
    case dict:find(NativeType,Pool) of
        {ok,{-1, TypeDef}} ->
            %% The type needs hashing and it's reference needs updating
            {Ref,NewTypeDef,NewState} = assign_reference(TypeDef, State),
%%            Hash = erlang:phash2(TypeDef),
            %%%%%%%%%%%%%%%%%%%%%%%%%%
            %% LOOK INTO THIS DEPENDENCY, MAYBE EXTRACT IT OUT
%%            type_decoding:hash_store(NewTypeDef,NewState),  %% 貌似这个没用,可以去掉.
            %%%%%%%%%%%%%%%%%%%%%%%%%%
            {hash, Ref,NewTypeDef , NewState};
        {ok,{Ref, TypeDef} } ->
            {ref, Ref};
        error ->
            case get_deftype_public_pool(NativeType) of
                undefined ->
                    throw("unkonw native type "++ atom_to_list(NativeType));
                TypeDefTmp ->
                    State2 = enlist(TypeDefTmp,State),
                    visit(NativeType,State2)
            end
    end.

%% This increments the reference count for the current scope and updates the
%% reference in the pool of known types
assign_reference(TypeDef = #type_def{native_type = Key},
                 #encoding_state{pool = OldPool, count = Count}) ->
    NewCount = Count + 1,
    NewTypeDef = TypeDef#type_def{defineNo = NewCount},
    Value = {NewCount, NewTypeDef},
    NewPool = dict:store(Key, Value, OldPool),
    logger:debug("[encode] assign_reference type ~p definedNo ~p",[Key,NewCount]),
    {NewCount,NewTypeDef,#encoding_state{pool = NewPool, count = NewCount}}.

get_deftype_public_pool(NativeType)->
    type_register:lookup_native_type(NativeType).