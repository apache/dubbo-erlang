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
-module(dubbo_serializa_hessian).

-include("dubbo.hrl").
%% API
-export([decode_header/1]).
-export([decode_response/2]).
-export([decode_request/2]).
-export([encode_request_data/1, encode_response_data/1]).


encode_request_data(Request) ->
    State = dubbo_type_encoding:init(),
    DataType = case Request#dubbo_request.is_event of
                   true ->
                       dubbo_event;
                   false ->
                       case Request#dubbo_request.data of
                           #dubbo_rpc_invocation{} ->
                               dubbo_rpc_invocation;
                           _ ->
                               unknow
                       end
               end,
    {ok, Bin} = encode_request_data(DataType, Request, Request#dubbo_request.data, State),
    {ok, Bin}.

encode_request_data(dubbo_event, _Request, Data, State) ->
    Bin = cotton_hessian:encode(Data, State),
    {ok, Bin};
encode_request_data(dubbo_rpc_invocation, _Request, Data, State) ->
    METHOD_NAME = Data#dubbo_rpc_invocation.methodName,
    METHOD_ARGS_TYPES = Data#dubbo_rpc_invocation.parameterDesc,
    RequestList = [
        cotton_hessian:encode(?DUBBO_VERSION, State), %% dubbo version
        cotton_hessian:encode(Data#dubbo_rpc_invocation.className, State),
        cotton_hessian:encode(Data#dubbo_rpc_invocation.classVersion, State),
        cotton_hessian:encode(METHOD_NAME, State),
        cotton_hessian:encode(METHOD_ARGS_TYPES, State)
    ],
    {ArgsBin, State2} = encode_arguments(Data, State),
    AttachDict = dict:from_list(Data#dubbo_rpc_invocation.attachments),
    AttachMaps = #map{dict = AttachDict},
    {AttachBinay, _} = cotton_hessian:encode(AttachMaps, State2),
    RequestData = erlang:iolist_to_binary(RequestList ++ [ArgsBin, AttachBinay]),
    {ok, RequestData}.


encode_response_data(Response) ->
    State = dubbo_type_encoding:init(),
    DataType = case Response#dubbo_response.is_event of
                   true ->
                       dubbo_event;
                   false ->
                       case Response#dubbo_response.data of
                           #dubbo_rpc_invocation{} ->
                               dubbo_rpc_invocation;
                           _ ->
                               unknow
                       end
               end,
    {ok, Bin} = encode_response_data(DataType, Response, Response#dubbo_response.data, State),
    {ok, Bin}.

encode_response_data(dubbo_event, _Response, Data, State) ->
    Bin = cotton_hessian:encode(Data, State),
    {ok, Bin};
encode_response_data(dubbo_rpc_invocation, _Response, Data, State) ->
    Result = case Data of
                 null ->
                     [
                         cotton_hessian:encode(?RESPONSE_NULL_VALUE, State)
                     ];
                 _ ->
                     {ArgsBin, _State2} = encode_arguments(Data, State),
                     [
                         cotton_hessian:encode(?RESPONSE_VALUE, State),
                         ArgsBin
                     ]
             end,
    ResponseData = erlang:iolist_to_binary(Result),
    {ok, ResponseData}.

encode_arguments(Data, State) ->
    {StateNew} = lists:foldl(fun(X, {StateTmp}) ->
        StateTmpNew = dubbo_type_encoding:enlist(X, StateTmp),
        {StateTmpNew} end,
        {State}, Data#dubbo_rpc_invocation.parameterTypes),
    {Bin, State2} = lists:foldl(fun(X, {BinTmp, StateTmp2}) ->
        case cotton_hessian:encode(X, StateTmp2) of
            {ArgsBin, StateTmpNew} ->
                {<<BinTmp/binary, ArgsBin/binary>>, StateTmpNew};
            ArgsBin2 ->
                {<<BinTmp/binary, ArgsBin2/binary>>, StateTmp2}
        end end,
        {<<>>, StateNew}, Data#dubbo_rpc_invocation.parameters),
    {Bin, State2}.

-spec decode_header(binary()) -> {State :: ok|error, Type :: request|response, Data :: #dubbo_response{}|#dubbo_request{}}.
decode_header(Header) ->
    <<?DUBBO_MEGIC_HIGH, ?DUBBO_MEGIC_LOW, Flag:8, State:8, Mid:64, DataLen:32>> = Header,
    if
        (Flag band 16#80) == 0 ->
            {DecodeState, Res} = decode_header(response, Flag, State, Mid, DataLen),
            {DecodeState, response, Res};
        true ->
            {DecodeState, Req} = decode_header(request, Flag, State, Mid, DataLen),
            {DecodeState, request, Req}
    end.
decode_header(request, Flag, State, Mid, DataLen) ->
    SerializeType = Flag band 16#1f,
    IsTwoWay = if
                   (Flag band 16#40) /= 0 -> true;
                   true -> false
               end,
    IsEvent = if
                  (Flag band 16#20) /= 0 -> true;
                  true -> false
              end,
    Req = #dubbo_request{
        is_event = IsEvent,
        is_twoway = IsTwoWay,
        mid = Mid,
        mversion = <<"2.0.0">>,
        serialize_type = SerializeType
    },
    {ok, Req};
decode_header(response, Flag, State, Mid, DataLen) ->
    SerializeType = Flag band 16#1f,
    IsEvent = if
                  (Flag band 16#20) /= 0 -> true;
                  true -> false
              end,
    Res = #dubbo_response{is_event = IsEvent,
        serialize_type = SerializeType,
        state = State,
        mid = Mid
    },
    {ok, Res}.

-spec decode_response(#dubbo_response{}, binary()) -> {ok, #dubbo_response{}}.
decode_response(Res, Data) ->
    if
        Res#dubbo_response.is_event == true ->
            decode_response(dubbo_event, Res, Data);
        true ->
            decode_response(dubbo_rpc_invocation, Res, Data)
    end.

decode_response(dubbo_rpc_invocation, Res, Data) ->
    {Rest, Type, State} = cotton_hessian:decode(Data, cotton_hessian:init()),
    case Type of
        1 ->
            {_, Object, DecodeState} = cotton_hessian:decode(Rest, State),
            {ok, Res#dubbo_response{data = Object, decode_state = DecodeState}};
        2 ->
            {ok, Res#dubbo_response{data = null, decode_state = State}};
        _ ->
            logger:warning("decode unkonw type ~p ~p", [Type, Rest]),
            {Rest2, Object2, DecodeState2} = cotton_hessian:decode(Rest, State),
            logger:warning("decode unkonw type2 ~p ~p", [Object2, Rest2]),
            {ok, Res#dubbo_response{data = Object2, decode_state = DecodeState2}}
    end;
decode_response(dubbo_event, Res, Data) ->
    {_Rest, undefined, _NewState} = cotton_hessian:decode(Data, cotton_hessian:init()),
    {ok, Res#dubbo_response{data = undefined}}.

-spec decode_request(#dubbo_request{}, binary()) -> {ok, #dubbo_request{}}.
decode_request(Req, Data) ->
    if
        Req#dubbo_request.is_event == true ->
            decode_request(dubbo_event, Req, Data);
        true ->
            decode_request(dubbo_rpc_invocation, Req, Data)
    end.

decode_request(dubbo_rpc_invocation, Req, Data) ->
    {ResultList, _NewState, _RestData} = decode_request_body(Data, cotton_hessian:init(), [dubbo, path, version, method_name, desc_and_args, attachments]),
    [_DubboVersion, Path, Version, MethodName, Desc, ArgsObj, Attachments] = ResultList,
    RpcData = #dubbo_rpc_invocation{className = Path, classVersion = Version, methodName = MethodName, parameterDesc = Data, parameters = ArgsObj, attachments = Attachments},
    Req2 = Req#dubbo_request{data = RpcData},
    {ok, Req2};
decode_request(dubbo_event, Req, Data) ->
    {_Rest, EventData, _NewState} = cotton_hessian:decode(Data, cotton_hessian:init()),
    {ok, Req#dubbo_request{data = EventData}}.

decode_request_body(Data, State, List) ->
    {ResultList, NewState, RestData} = decode_request_body(List, Data, State, []),
    {lists:reverse(ResultList), NewState, RestData}.
decode_request_body([ParseType | List], Data, State, ResultList)
    when ParseType == dubbo;ParseType == path;ParseType == version;ParseType == method_name ->
    {Rest, Result, NewState} = cotton_hessian:decode(Data, State),
    decode_request_body(List, Rest, NewState, [Result] ++ ResultList);
decode_request_body([desc_and_args | List], Data, State, ResultList) ->
    {Rest, ParameterDesc, State1} = cotton_hessian:decode(Data, State),
    if
        size(ParameterDesc) == 0 ->
            decode_request_body(List, Rest, State1, [[], []] ++ ResultList);
        true ->
            ParameterDescArray = binary:split(ParameterDesc, <<";">>),
            {ArgsObjList, NewState, RestData} = decode_request_body_args(ParameterDescArray, Rest, State1, []),
            decode_request_body(List, RestData, NewState, [ArgsObjList, ParameterDesc] ++ ResultList)
    end;
decode_request_body([attachments | List], Data, State, ResultList) ->
    {Rest, Attachments, State1} = cotton_hessian:decode(Data, State),
    AttachmentsList = dict:to_list(Attachments#map.dict),
    decode_request_body(List, Rest, State1, [AttachmentsList] ++ ResultList);
decode_request_body([_Type1 | List], Data, State, ResultList) ->
    logger:warning("decode_request_body unknow type"),
    decode_request_body(List, Data, State, ResultList);
decode_request_body([], Data, State, ResultList) ->
    {ResultList, State, Data}.

decode_request_body_args([], Data, State, ArgsObjList) ->
    {ArgsObjList, State, Data};
decode_request_body_args([ArgsType | RestList], Data, State, ArgsObjList) when ArgsType == <<>> ->
    decode_request_body_args(RestList, Data, State, ArgsObjList);
decode_request_body_args([_ArgsType | RestList], Data, State, ArgsObjList) ->
    {Rest, ArgObj, NewState} = cotton_hessian:decode(Data, State),
    ArgObj2 = dubbo_type_transfer:classobj_to_native(ArgObj, NewState),
    decode_request_body_args(RestList, Rest, NewState, ArgsObjList ++ [ArgObj2]).