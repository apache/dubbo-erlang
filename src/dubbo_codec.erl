%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2016 下午6:21
%%%-------------------------------------------------------------------
-module(dubbo_codec).
-author("dlive").

-include("dubbo.hrl").
-include("hessian.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.
%% API
-export([encode_request/1,encode_response/1]).

-export([decode_header/1]).
-export([decode_response/2]).
-export([decode_request/2]).



-spec encode_request(#dubbo_request{})->{ok,binary()} | {error,term()}.
encode_request(Request)->
%%    logger:debug("encode request ~p",[Request]),
    {ok,RequestData} = encode_request_data(Request#dubbo_request.serialize_type,Request),
    Size = byte_size(RequestData),
    Header = encode_header(Request,Size,0),
    RequestContent = <<Header/binary,RequestData/binary>>,
    {ok,RequestContent}.

encode_header(Request,DataLen,RequestState)->
    Header2=-128 bor Request#dubbo_request.serialize_type,
    Header21=case Request#dubbo_request.is_twoway of
        true -> Header2 bor 64;
        false-> Header2
    end,
    Header22=case Request#dubbo_request.is_event of
                 true -> Header21 bor 32;
                 false-> Header21
             end,
    RequestId = Request#dubbo_request.mid,
    Header = << ?DUBBO_MEGIC:16,Header22:8,RequestState:8,RequestId:64,DataLen:32>>,
    Header.
encode_request_data(?SERIALIZATION_FASTJSON,Request)->
    dubbo_serializa_json:encode_request_data(Request);

encode_request_data(?SERIALIZATION_HESSIAN,Request)->
    dubbo_serializa_hessian:encode_request_data(Request).


-spec encode_response(#dubbo_response{})-> {ok,term()}.
encode_response(Response)->
    {ok,ResponseData} = encode_response_data(Response#dubbo_response.serialize_type,Response),
    Size = byte_size(ResponseData),
    Header = encode_response_header(Response,Size,?RESPONSE_STATE_OK),
    ResponseContent = <<Header/binary,ResponseData/binary>>,
    {ok, ResponseContent}.

encode_response_data(?SERIALIZATION_FASTJSON,Response)->
    {ok,Bin} = dubbo_serializa_json:encode_response_data(Response),
    {ok,Bin};
encode_response_data(?SERIALIZATION_HESSIAN,Response)->

    {ok,Bin} = dubbo_serializa_hessian:encode_response_data(Response),
    {ok,Bin}.

encode_response_header(Response,DataLen, ResponseState)->
    Header2= Response#dubbo_response.serialize_type,
    Header21=case Response#dubbo_response.is_twoway of
                 true -> Header2 bor 64;
                 false-> Header2
             end,
    Header22=case Response#dubbo_response.is_event of
                 true -> Header21 bor 32;
                 false-> Header21
             end,
    RequestId = Response#dubbo_response.mid,
    Header = << ?DUBBO_MEGIC:16,Header22:8, ResponseState:8,RequestId:64,DataLen:32>>,
    Header.


-spec decode_header(binary())-> {State::ok|error,Type::request|response,Data::#dubbo_response{}|#dubbo_request{}}.
decode_header(Header)->
    <<?DUBBO_MEGIC_HIGH,?DUBBO_MEGIC_LOW,Flag:8,State:8,Mid:64,DataLen:32>> = Header,
    if
        (Flag band 16#80) == 0 ->
            {DecodeState,Res} = decode_header(response,Flag,State,Mid,DataLen),
            {DecodeState,response,Res};
        true ->
            {DecodeState,Req} = decode_header(request,Flag,State,Mid,DataLen),
            {DecodeState,request,Req}
    end.
decode_header(request,Flag,State,Mid,DataLen)->
    SerializeType = Flag band 16#1f,
    IsTwoWay = if
                   (Flag band 16#40) /=0 -> true;
                   true -> false
    end,
    IsEvent = if
                  (Flag band 16#20) /=0 -> true;
                  true -> false
              end,
    Req = #dubbo_request{
        is_event = IsEvent,
        is_twoway = IsTwoWay,
        mid = Mid,
        mversion = <<"2.0.0">>,
        serialize_type = SerializeType
    },
    {ok,Req};
decode_header(response,Flag,State,Mid,DataLen)->
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
    {ok,Res}.

-spec decode_response(#dubbo_response{},binary())-> {ok,#dubbo_response{}}.
decode_response(Res,Data)->
    case Res#dubbo_response.serialize_type of
        ?SERIALIZATION_FASTJSON ->
            dubbo_serializa_json:decode_response(Res,Data);
        ?SERIALIZATION_HESSIAN ->
            dubbo_serializa_hessian:decode_response(Res,Data)
    end.

%%decode_response(?SERIALIZATION_FASTJSON,dubbo_rpc_invocation,Res,Data)->
%%    dubbo_serializa_json:decode_response(dubbo_rpc_invocation,Res,Data);
%%
%%decode_response(?SERIALIZATION_HESSIAN,dubbo_rpc_invocation,Res,Data)->
%%    {Rest,Type,State} = cotton_hessian:decode(Data,cotton_hessian:init()),
%%    case Type of
%%        1 ->
%%            {_,Object,DecodeState} = cotton_hessian:decode(Rest,State),
%%            {ok,Res#dubbo_response{data = Object,decode_state = DecodeState}};
%%        2 ->
%%            {ok,Res#dubbo_response{data = null,decode_state = State}};
%%        _->
%%            logger:warning("decode unkonw type ~p ~p",[Type,Rest]),
%%            {Rest2,Object2,DecodeState2} = cotton_hessian:decode(Rest,State),
%%            logger:warning("decode unkonw type2 ~p ~p",[Object2,Rest2]),
%%            {ok,Res#dubbo_response{data = Object2,decode_state = DecodeState2}}
%%    end;
%%decode_response(?SERIALIZATION_HESSIAN,dubbo_event,Res,Data)->
%%    {_Rest,undefined,_NewState} = cotton_hessian:decode(Data,cotton_hessian:init()),
%%    {ok,Res#dubbo_response{data = undefined}}.

-spec decode_request(#dubbo_request{},binary())-> {ok,#dubbo_request{}}.
decode_request(Req,Data)->
    case Req#dubbo_request.serialize_type of
        ?SERIALIZATION_FASTJSON ->
            dubbo_serializa_json:decode_request(Req,Data);
        ?SERIALIZATION_HESSIAN ->
            dubbo_serializa_hessian:decode_request(Req,Data)
    end.

%%decode_request(dubbo_rpc_invocation,Req,Data)->
%%    {ResultList,NewState,RestData} = decode_request_body(Data,cotton_hessian:init(),[dubbo,path,version,method_name,desc_and_args,attachments]),
%%    [DubboVersion,Path,Version,MethodName,Desc,ArgsObj,Attachments]=ResultList,
%%    RpcData = #dubbo_rpc_invocation{className = Path,classVersion = Version,methodName = MethodName,parameterDesc = Data,parameters = ArgsObj,attachments = Attachments},
%%    Req2 = Req#dubbo_request{data = RpcData},
%%    {ok,Req2};
%%
%%decode_request(dubbo_event,Req,Data)->
%%    {_Rest,undefined,_NewState} = cotton_hessian:decode(Data,cotton_hessian:init()),
%%    {ok,Req#dubbo_request{data = undefined}}.
%%
%%decode_request_body(Data,State,List)->
%%    {ResultList,NewState,RestData} = decode_request_body(List,Data,State,[]),
%%    {lists:reverse(ResultList),NewState,RestData}.
%%decode_request_body([ParseType|List],Data,State,ResultList)
%%    when ParseType==dubbo;ParseType==path;ParseType==version;ParseType==method_name ->
%%    {Rest,Result,NewState } = cotton_hessian:decode(Data,State),
%%    decode_request_body(List,Rest,NewState, [Result] ++ ResultList);
%%decode_request_body([desc_and_args| List],Data,State,ResultList)->
%%    {Rest,ParameterDesc,State1 } = cotton_hessian:decode(Data,State),
%%    if
%%        size(ParameterDesc) == 0 ->
%%            decode_request_body(List,Rest,State1, [ [],[] ]++ ResultList);
%%        true ->
%%            ParameterDescArray = binary:split(ParameterDesc,<<";">>),
%%            {ArgsObjList,NewState,RestData} = decode_request_body_args(ParameterDescArray,Rest,State1,[]),
%%            decode_request_body(List,RestData,NewState, [ArgsObjList,ParameterDesc]++ ResultList)
%%    end;
%%decode_request_body([attachments|List],Data,State,ResultList)->
%%    {Rest,Attachments,State1 } = cotton_hessian:decode(Data,State),
%%    AttachmentsList = dict:to_list(Attachments#map.dict),
%%    decode_request_body(List,Rest,State1,[AttachmentsList] ++ ResultList);
%%decode_request_body([_Type1|List],Data,State,ResultList)->
%%    logger:warning("decode_request_body unknow type"),
%%    decode_request_body(List,Data,State, ResultList);
%%decode_request_body([],Data,State,ResultList)->
%%    {ResultList,State,Data}.
%%
%%decode_request_body_args([],Data,State,ArgsObjList)->
%%    {ArgsObjList,State,Data};
%%decode_request_body_args([ArgsType|RestList],Data,State,ArgsObjList) when ArgsType== <<>> ->
%%    decode_request_body_args(RestList,Data,State,ArgsObjList);
%%decode_request_body_args([_ArgsType|RestList],Data,State,ArgsObjList) ->
%%    {Rest,ArgObj,NewState } = cotton_hessian:decode(Data,State),
%%    ArgObj2 = de_type_transfer:classobj_to_native(ArgObj,NewState),
%%    decode_request_body_args(RestList,Rest,NewState,ArgsObjList++[ArgObj2]).