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
-include("dubboerl.hrl").
%% API
-export([parse_provider_info/1, gen_provider_info/1]).

parse_provider_info(#dubbo_url{scheme = Scheme, host = Host, port = Port, parameters = Parameters}) ->
    ProviderInfo = #provider_config{protocol = Scheme, host = Host, port = Port},
    maps:to_list(Parameters),
    ProviderConfig = maps:fold(
        fun(K, V, Acc1) ->
            parse_parameter(K, V, Acc1)
        end, ProviderInfo, Parameters),
    logger:debug("parse provider,result: ~p", [ProviderConfig]),
    {ok, ProviderConfig}.

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
parse_parameter(<<"anyhost">>, Value, Config) ->
    Config#provider_config{anyhost = binary_to_existing_atom(Value, latin1)};
parse_parameter(<<"application">>, Value, Config) ->
    Config#provider_config{application = Value};
parse_parameter(<<"dubbo">>, Value, Config) ->
    Config#provider_config{dubbo = Value};
parse_parameter(<<"executes">>, Value, Config) ->
    Config#provider_config{executes = binary_to_integer(Value)};
parse_parameter(<<"interface">>, Value, Config) ->
    Config#provider_config{interface = Value};
parse_parameter(<<"methods">>, Value, Config) ->
    MethodList = binary:split(Value, <<",">>, [global, trim_all]),
    Config#provider_config{methods = MethodList};
parse_parameter(<<"side">>, Value, Config) ->
    Config#provider_config{side = Value};
parse_parameter(<<"interface">>, Value, Config) ->
    Config#provider_config{interface = Value};
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