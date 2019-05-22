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
-module(dubbo_serializa_json).


-include("dubbo.hrl").
%% API
-export([decode_response/2, decode_request/2, decode_header/1, decode_request/2]).

-export([encode_request_data/1, encode_response_data/1]).

encode_request_data(Request) ->
    DataType = case Request#dubbo_request.is_event of
                   false ->
                       dubbo_rpc_invocation;
                   true ->
                       dubbo_event
               end,
    {ok, Bin} = encode_request_data(DataType, Request, Request#dubbo_request.data, []),
    {ok, Bin}.


encode_request_data(dubbo_rpc_invocation, _Request, Data, State) ->
    RequestList = [
        string_encode(?DUBBO_VERSION),
        ?LINE_SEPERATOR,
        string_encode(Data#dubbo_rpc_invocation.className),
        ?LINE_SEPERATOR,
        string_encode(Data#dubbo_rpc_invocation.classVersion),
        ?LINE_SEPERATOR,
        string_encode(Data#dubbo_rpc_invocation.methodName),
        ?LINE_SEPERATOR,
        string_encode(Data#dubbo_rpc_invocation.parameterDesc),
        ?LINE_SEPERATOR
    ],
    {ArgsBin, _} = encode_arguments(Data, State),
    AttachBinay = jiffy:encode({Data#dubbo_rpc_invocation.attachments}, []),
    RequestData = erlang:iolist_to_binary(RequestList ++ [ArgsBin, AttachBinay, ?LINE_SEPERATOR]),
    {ok, RequestData};
encode_request_data(dubbo_event, _Request, Data, _State) ->
    %% @todo 确认该数据类型
    Bin = jiffy:encode(Data),
    {ok, Bin}.


encode_response_data(Response) ->
    State = #{},
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
    Bin = jiffy:encode(Data, []),
    {ok, Bin};
encode_response_data(dubbo_rpc_invocation, _Response, Data, State) ->
    Result = case Data of
                 null ->
                     [
                         string_encode(?RESPONSE_NULL_VALUE)
                     ];
                 _ ->
                     {ArgsBin, _State2} = encode_arguments(Data, State),
                     [
                         string_encode(?RESPONSE_VALUE),
                         ?LINE_SEPERATOR,
                         ArgsBin
                     ]
             end,
    ResponseData = erlang:iolist_to_binary(Result),
    {ok, ResponseData}.

encode_arguments(Data, State) ->
    {Bin} = lists:foldl(
        fun(X, {BinTmp}) ->
            ArgsBin = string_encode(X),
            {<<BinTmp/binary, ArgsBin/binary, ?LINE_SEPERATOR/binary>>}
        end,
        {<<>>}, Data#dubbo_rpc_invocation.parameters),
    {Bin, State}.

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
    DataList = binary:split(Data, <<"\n">>, [global]),
    [TypeBin | DataList1] = DataList,
%%    {Rest,Type,State} = cotton_hessian:decode(Data,cotton_hessian:init()),
    Type = jiffy:decode(TypeBin),

    case Type of
        ?RESPONSE_VALUE ->
%%            {_,Object,DecodeState} = cotton_hessian:decode(Rest,State),
            [Value | _] = DataList1,
            Object = jiffy:decode(Value, [return_maps]),
            {ok, Res#dubbo_response{data = Object}};
        ?RESPONSE_NULL_VALUE ->
            {ok, Res#dubbo_response{data = null}};
        ?RESPONSE_WITH_EXCEPTION ->
            [ExceptionValue | _] = DataList1,
            ExceptionObject = jiffy:decode(ExceptionValue),
            {ok, Res#dubbo_response{data = ExceptionObject}};
        Other ->
            logger:error("server response unkonw info ~p", [Other]),
            {ok, Res#dubbo_response{data = <<"server pool exhausted">>}}

    end;
decode_response(dubbo_event, Res, Data) ->
%%    {_Rest,undefined,_NewState} = cotton_hessian:decode(Data,cotton_hessian:init()),
    {ok, Res#dubbo_response{data = null}}.

-spec decode_request(#dubbo_request{}, binary()) -> {ok, #dubbo_request{}}.
decode_request(Req, Data) ->
    if
        Req#dubbo_request.is_event == true ->
            decode_request(dubbo_event, Req, Data);
        true ->
            decode_request(dubbo_rpc_invocation, Req, Data)
    end.

decode_request(dubbo_rpc_invocation, Req, Data) ->
    {ResultList, NewState, RestData} = decode_request_body(Data, #{}, [dubbo, path, version, method_name, desc_and_args, attachments]),
    [DubboVersion, Path, Version, MethodName, Desc, ArgsObj, Attachments] = ResultList,
    RpcData = #dubbo_rpc_invocation{className = Path, classVersion = Version, methodName = MethodName, parameterDesc = Desc, parameters = ArgsObj, attachments = Attachments},
    Req2 = Req#dubbo_request{data = RpcData},
    {ok, Req2};

decode_request(dubbo_event, Req, Data) ->
%%    DataList = binary:split(Data,<<"\n">>),
    logger:debug("dubbo_event datalist ~w", [Data]),
    Result = jiffy:decode(Data, []),
%%    {_Rest,undefined,_NewState} = cotton_hessian:decode(Data,cotton_hessian:init()),
    {ok, Req#dubbo_request{data = Result}}.

decode_request_body(Data, State, List) ->
    logger:debug("decode_request_body origin data ~p", [Data]),
    DataList = binary:split(Data, <<"\n">>, [global]),
    if
        length(DataList) < 6 ->
            {error, request_data_error};
        true ->
            {ResultList, NewState, RestData} = decode_request_body(List, DataList, State, []),
            {lists:reverse(ResultList), NewState, RestData}
    end.



decode_request_body([ParseType | List], [DataItem | Data], State, ResultList)
    when ParseType == dubbo;ParseType == path;ParseType == version;ParseType == method_name ->
    DecodeData = jiffy:decode(DataItem, [return_maps]),
    decode_request_body(List, Data, State, [DecodeData] ++ ResultList);

decode_request_body([desc_and_args | List], [DescBin | Data], State, ResultList) ->
    ParameterDesc = jiffy:decode(DescBin, []),

%%    {Rest,ParameterDesc,State1 } = cotton_hessian:decode(Data,State),
    if
        size(ParameterDesc) == 0 ->
            decode_request_body(List, Data, State, [[], []] ++ ResultList);
        true ->
            ParameterDescArray = binary:split(ParameterDesc, <<";">>),
            {ArgsObjList, NewState, RestData} = decode_request_body_args(ParameterDescArray, Data, State, []),
            decode_request_body(List, RestData, NewState, [ArgsObjList, ParameterDesc] ++ ResultList)
    end;
decode_request_body([attachments | List], [DataItem | Data], State, ResultList) ->
    Attachments = jiffy:decode(DataItem, [return_maps]),
%%    AttachmentsList = dict:to_list(Attachments#map.dict),
    decode_request_body(List, Data, State, [Attachments] ++ ResultList);
decode_request_body([_Type1 | List], Data, State, ResultList) ->
    logger:warning("decode_request_body unknow type"),
    decode_request_body(List, Data, State, ResultList);
decode_request_body([], Data, State, ResultList) ->
    {ResultList, State, Data}.


decode_request_body_args([], Data, State, ArgsObjList) ->
    {ArgsObjList, State, Data};

decode_request_body_args([ArgsType | RestList], Data, State, ArgsObjList) when ArgsType == <<>> ->
    decode_request_body_args(RestList, Data, State, ArgsObjList);

decode_request_body_args([ArgsType | RestList], [DataItem | Data], State, ArgsObjList) ->
    ArgObj = jiffy:decode(DataItem, [return_maps]),
%%    {Rest,ArgObj,NewState } = cotton_hessian:decode(Data,State),
    ArgObj2 = dubbo_type_transfer:jsonobj_to_native(ArgsType, ArgObj, State),
    decode_request_body_args(RestList, Data, State, ArgsObjList ++ [ArgObj2]).

string_encode(Data) when is_binary(Data) ->
    <<<<"\"">>/binary, Data/binary, <<"\"">>/binary>>;
string_encode(Data) when is_tuple(Data) ->
    [Name | _] = tuple_to_list(Data),
%%    Size = record_info(size, Name),
%%    Fields = record_info(fields, Name),
    case type_register:lookup_native_type(Name) of
        undefined ->
            <<"data encode error">>;
        #type_def{fieldnames = Fields, foreign_type = _ForeignType} ->
            logger:debug("string_encode lookup ~p ret ~p", [Name, Fields]),
            MapValue = lists:foldl(
                fun({I, E}, Acc) ->
                    Acc#{E => element(I, Data)}
                end, #{}, lists:zip(lists:seq(2, length(Fields) + 1), Fields)),
            jiffy:encode(MapValue)
    end;

string_encode(Data) ->
    jiffy:encode(Data).
