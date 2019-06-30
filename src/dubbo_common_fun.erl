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
-module(dubbo_common_fun).

-include("dubboerl.hrl").
%% API
-export([local_ip_v4/0, local_ip_v4_str/0, parse_url/1, url_to_binary/1, parse_url_parameter/1, binary_list_join/2]).

-define(URL_PATH_SEPARATOR,47).  %% 47 == <<"/">>

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127, 0, 0, 1}
    ]).

local_ip_v4_str() ->
    {V1, V2, V3, V4} = local_ip_v4(),
    list_to_binary(io_lib:format("~p.~p.~p.~p", [V1, V2, V3, V4])).


-spec(parse_url(Url :: binary()|list()) -> {ok, #dubbo_url{}}|{error, any()}).
parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url(Url) ->
    case http_uri:parse(Url, []) of
        {ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
            QueryStr = case lists:prefix("?", Query) of
                           true ->
                               [_ | Query2] = Query,
                               Query2;
                           false ->
                               Query
                       end,
            Parameters = parse_url_parameter(QueryStr),
            Result = #dubbo_url{
                scheme = atom_to_binary(Scheme, utf8),
                user_info = UserInfo,
                host = list_to_binary(Host),
                port = Port,
                path = Path,
                parameters = Parameters
            },
            {ok, Result};
        {error, R1} ->
            {error, R1}
    end.


parse_url_parameter(ParameterStr) when is_binary(ParameterStr) ->
    parse_url_parameter(binary_to_list(ParameterStr));
parse_url_parameter(ParameterStr) ->
    QueryListTmp = string:tokens(ParameterStr, "&"),
    parse_url_parameter(QueryListTmp, #{}).

parse_url_parameter([], Parameters) ->
    Parameters;
parse_url_parameter([Item | Rest], Parameters) ->
    case string:tokens(Item, "=") of
        KeyPair when length(KeyPair) == 2 ->
            [Key, Value] = KeyPair,
            parse_url_parameter(Rest, maps:put(list_to_binary(Key), list_to_binary(Value), Parameters));
        KeyPair2 ->
            logger:error("parse parameter error, keypair ~p", [KeyPair2]),
            parse_url_parameter(Rest, Parameters)
    end.


url_to_binary(UrlInfo) ->
    ParameterStr = format_parameter(UrlInfo#dubbo_url.parameters),
    Value = lists:flatten(io_lib:format(<<"~s://~s:~p/~s?~s">>,
        [
            UrlInfo#dubbo_url.scheme,
            UrlInfo#dubbo_url.host,
            UrlInfo#dubbo_url.port,
            format_path(UrlInfo#dubbo_url.path),
            ParameterStr
        ])),
    list_to_binary(Value).
format_path(<< ?URL_PATH_SEPARATOR:8,Rest/binary>>) ->
    logger:debug("format_path1 ~p",[Rest]),
    Rest;
format_path([?URL_PATH_SEPARATOR|Rest]) ->
    logger:debug("format_path3 ~p",[Rest]),
    Rest;
format_path(Value) ->
    logger:debug("format_path2 ~p",[Value]),
    Value.

format_parameter(undefined) ->
    "";
format_parameter(Parameter) when is_map(Parameter) ->
    KeyValues = maps:to_list(Parameter),
    format_parameter(KeyValues);
format_parameter(Parameters) ->
    KeyValues2 = [io_lib:format("~s=~s", [Key, Value]) || {Key, Value} <- Parameters],
    ParameterStr1 = string:join(KeyValues2, "&"),
    ParameterStr1.

binary_list_join([], _Separator) ->
    <<"">>;
binary_list_join([H | T], Separator) ->
    binary_list_join1(H, T, Separator).

binary_list_join1(Header, [], _Separator) ->
    Header;
binary_list_join1(Header, [Item | Rest], Separator) ->
    binary_list_join1(<<Header/binary, Separator/binary, Item/binary>>, Rest, Separator).

