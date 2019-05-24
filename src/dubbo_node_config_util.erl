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
-module(dubbo_node_config_util).

-include("dubbo.hrl").
%% API
-export([parse_provider_info/1, gen_provider_info/1]).

parse_provider_info(ProviderStr) when is_binary(ProviderStr) ->
    parse_provider_info(binary_to_list(ProviderStr));
parse_provider_info(ProviderStr) ->
    case http_uri:parse(http_uri:decode(ProviderStr), [{scheme_defaults, [{dubbo, 20880}]}]) of
        {ok, {Scheme, _UserInfo, Host, Port, _Path, Query}} ->
            QueryStr = case lists:prefix("?", Query) of
                           true ->
                               [_ | Query2] = Query,
                               Query2;
                           false ->
                               Query
                       end,
            QueryListTmp = string:tokens(QueryStr, "&"),
            ProviderConfig = parse_parameter(QueryListTmp, #provider_config{protocol = Scheme, host = Host, port = Port}),
            logger:debug("parse provider info string ~p,result: ~p", [ProviderStr, ProviderConfig]),
            {ok, ProviderConfig};
        {error, R1} ->
            logger:debug("parse provider error string ~p, error ~p", [ProviderStr, R1]),
            {error, R1}
    end.


parse_parameter([], Config) ->
    Config;
parse_parameter([Item | Rest], Config) ->
    case string:tokens(Item, "=") of
        KeyPair when length(KeyPair) == 2 ->
            [Key, Value] = KeyPair,
            ConfigNew = parse_parameter(Key, Value, Config),
            parse_parameter(Rest, ConfigNew);
        KeyPair2 ->
            logger:error("parse parameter error, keypair ~p", [KeyPair2]),
            parse_parameter(Rest, Config)
    end.
parse_parameter("anyhost", Value, Config) ->
    Config#provider_config{anyhost = list_to_atom(Value)};
parse_parameter("application", Value, Config) ->
    Config#provider_config{application = list_to_binary(Value)};
parse_parameter("dubbo", Value, Config) ->
    Config#provider_config{dubbo = list_to_binary(Value)};
parse_parameter("executes", Value, Config) ->
    Config#provider_config{executes = list_to_integer(Value)};
parse_parameter("interface", Value, Config) ->
    Config#provider_config{interface = list_to_binary(Value)};
parse_parameter("methods", Value, Config) ->
    MethodList = string:tokens(Value, ","),
    MethodList2 = [list_to_binary(Item) || Item <- MethodList],
    Config#provider_config{methods = MethodList2};
parse_parameter("side", Value, Config) ->
    Config#provider_config{side = list_to_binary(Value)};
parse_parameter("interface", Value, Config) ->
    Config#provider_config{interface = list_to_binary(Value)};
parse_parameter(_, _, Config) ->
    Config.

gen_provider_info(ProviderConfig) ->
    Parameter = gen_provider_parameter(ProviderConfig),
    Info = io_lib:format("dubbo://~s:~p/~s?~s", [
        ProviderConfig#provider_config.host,
        ProviderConfig#provider_config.port,
        ProviderConfig#provider_config.interface,
        Parameter
    ]),
    list_to_binary(http_uri:encode(Info)).

gen_provider_parameter(Providerconfig) ->
    Method = [binary_to_list(Item) || Item <- Providerconfig#provider_config.methods],
    Method2 = list_to_binary(string:join(Method, ",")),
    List = [
        {<<"interface">>, Providerconfig#provider_config.interface},
        {<<"application">>, Providerconfig#provider_config.application},
        {<<"anyhost">>, <<"true">>},
        {<<"dubbo">>, Providerconfig#provider_config.dubbo},
        {<<"executes">>, integer_to_binary(Providerconfig#provider_config.executes)},
        {<<"methods">>, Method2},
        {<<"side">>, Providerconfig#provider_config.side},
        {<<"timestamp">>, integer_to_binary(dubbo_time_util:timestamp_ms())}
    ],
    List2 = [io_lib:format("~ts=~ts", [Key, Value]) || {Key, Value} <- List],
    lists:flatten(string:join(List2, "&")).