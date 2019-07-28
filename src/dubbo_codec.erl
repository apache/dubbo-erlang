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
-module(dubbo_codec).

-include("dubbo.hrl").
-include("hessian.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.
%% API
-export([encode_request/1, encode_response/1]).

-export([decode_header/1]).
-export([decode_response/2]).
-export([decode_request/2]).



-spec encode_request(#dubbo_request{}) -> {ok, binary()} | {error, term()}.
encode_request(Request) ->
    {ok, RequestData} = encode_request_data(Request#dubbo_request.serialize_type, Request),
    Size = byte_size(RequestData),
    Header = encode_header(Request, Size, 0),
    RequestContent = <<Header/binary, RequestData/binary>>,
    {ok, RequestContent}.

encode_header(Request, DataLen, RequestState) ->
    Header2 = -128 bor Request#dubbo_request.serialize_type,
    Header21 = case Request#dubbo_request.is_twoway of
                   true -> Header2 bor 64;
                   false -> Header2
               end,
    Header22 = case Request#dubbo_request.is_event of
                   false -> Header21;
                   true -> Header21 bor 32
               end,
    RequestId = Request#dubbo_request.mid,
    Header = <<?DUBBO_MEGIC:16, Header22:8, RequestState:8, RequestId:64, DataLen:32>>,
    Header.
encode_request_data(?SERIALIZATION_FASTJSON, Request) ->
    dubbo_serializa_json:encode_request_data(Request);

encode_request_data(?SERIALIZATION_HESSIAN, Request) ->
    dubbo_serializa_hessian:encode_request_data(Request).


-spec encode_response(#dubbo_response{}) -> {ok, term()}.
encode_response(Response) ->
    {ok, ResponseData} = encode_response_data(Response#dubbo_response.serialize_type, Response),
    Size = byte_size(ResponseData),
    Header = encode_response_header(Response, Size, ?RESPONSE_STATE_OK),
    ResponseContent = <<Header/binary, ResponseData/binary>>,
    {ok, ResponseContent}.

encode_response_data(?SERIALIZATION_FASTJSON, Response) ->
    {ok, Bin} = dubbo_serializa_json:encode_response_data(Response),
    {ok, Bin};
encode_response_data(?SERIALIZATION_HESSIAN, Response) ->

    {ok, Bin} = dubbo_serializa_hessian:encode_response_data(Response),
    {ok, Bin}.

encode_response_header(Response, DataLen, ResponseState) ->
    Header2 = Response#dubbo_response.serialize_type,
    Header21 = case Response#dubbo_response.is_twoway of
                   true -> Header2 bor 64;
                   false -> Header2
               end,
    Header22 = case Response#dubbo_response.is_event of
                   false -> Header21;
                   true -> Header21 bor 32
               end,
    RequestId = Response#dubbo_response.mid,
    Header = <<?DUBBO_MEGIC:16, Header22:8, ResponseState:8, RequestId:64, DataLen:32>>,
    Header.


-spec(decode_header(Header :: binary()) -> {State :: ok|error, Type :: request|response, Data :: dubbo_response()|dubbo_request()}).
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
decode_header(request, Flag, _State, Mid, _DataLen) ->
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
decode_header(response, Flag, State, Mid, _DataLen) ->
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
    case Res#dubbo_response.serialize_type of
        ?SERIALIZATION_FASTJSON ->
            dubbo_serializa_json:decode_response(Res, Data);
        ?SERIALIZATION_HESSIAN ->
            dubbo_serializa_hessian:decode_response(Res, Data)
    end.


-spec decode_request(#dubbo_request{}, binary()) -> {ok, #dubbo_request{}}.
decode_request(Req, Data) ->
    case Req#dubbo_request.serialize_type of
        ?SERIALIZATION_FASTJSON ->
            dubbo_serializa_json:decode_request(Req, Data);
        ?SERIALIZATION_HESSIAN ->
            dubbo_serializa_hessian:decode_request(Req, Data)
    end.