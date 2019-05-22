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

-module(type_register).
%% API
-export([init/0, regiest_foreign_native/1, lookup_foreign_type/1, lookup_native_type/1]).
-include("hessian.hrl").
-define(FOREIGN_NATIVE_TABLE, foreign_native_table).
-define(NATIVE_FOREIGN_TABLE, native_foreign_table).

init() ->
    case ets:info(?FOREIGN_NATIVE_TABLE) of
        undefined ->
            ?FOREIGN_NATIVE_TABLE = ets:new(?FOREIGN_NATIVE_TABLE, [public, named_table]),
            logger:info("init decoding foreign_native_table table", []);
        _ ->
            ets:delete(?FOREIGN_NATIVE_TABLE),
            ?FOREIGN_NATIVE_TABLE = ets:new(?FOREIGN_NATIVE_TABLE, [public, named_table])
    end,
    case ets:info(?NATIVE_FOREIGN_TABLE) of
        undefined ->
            io:format("init decoding native_foreign_table table pid ~p~n", [self()]),
            ?NATIVE_FOREIGN_TABLE = ets:new(?NATIVE_FOREIGN_TABLE, [public, named_table]); %% public
        _ ->
            ets:delete(?NATIVE_FOREIGN_TABLE),
            ?NATIVE_FOREIGN_TABLE = ets:new(?NATIVE_FOREIGN_TABLE, [public, named_table])
    end,
    ok.


regiest_foreign_native(TypeDef) ->
    logger:debug("regiest foreign info ~p", [TypeDef]),
    ets:insert(?FOREIGN_NATIVE_TABLE, {TypeDef#type_def.foreign_type, TypeDef}),
    ets:insert(?NATIVE_FOREIGN_TABLE, {TypeDef#type_def.native_type, TypeDef}).


lookup_foreign_type(ForeignType) ->
    case ets:lookup(?FOREIGN_NATIVE_TABLE, ForeignType) of
        [] ->
            undefined;
        [{_, TypeDef}] ->
            TypeDef
    end.

lookup_native_type(NativeType) ->
    case ets:lookup(?NATIVE_FOREIGN_TABLE, NativeType) of
        [] ->
            undefined;
        [{_, TypeDef}] ->
            TypeDef
    end.