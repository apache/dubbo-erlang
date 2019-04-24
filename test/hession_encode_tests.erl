%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2016 上午10:46
%%%-------------------------------------------------------------------
-module(hession_encode_tests).
-author("dlive").

-include("hessian.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([object_test/0]).

-record(de_TestReq, {name, nick,age}).
-record(de_reg2, {reqinfo,age}).

object_test()->
    ForeignTypeA = <<"com.ifcoder.demo.bean.UserInfoRequest">>,
    TypeDefA = #type_def{foreign_type = ForeignTypeA,
        native_type = de_TestReq,
        fieldnames = record_info(fields,de_TestReq)},
    EncodingState0 = type_encoding:enlist(TypeDefA),
%%    ?debugFmt("state:~p~n",[EncodingState0]),
    RequestArg0 = #de_TestReq{name = <<"nameinfo">>, nick = <<"nickname">> ,age=10 },

    {Bin, State0} = hessianEncode:encode(RequestArg0, EncodingState0),

    type_register:init(),
    de_type_transfer:pre_process_typedef(de_TestReq,<<"com.ifcoder.demo.bean.UserInfoRequest">>,record_info(fields,de_TestReq)),
    {<<>>,Data,State2 } = hessianDecode2:decode(Bin,hessianDecode2:init()),
    DecodeResult = de_type_transfer:java_to_native(Data,State2),
    ?assert(is_record(DecodeResult,de_TestReq)),
    ?assertEqual(DecodeResult#de_TestReq.name,<<"nameinfo">>),
    ?assertEqual(DecodeResult#de_TestReq.nick,<<"nickname">>),
    ?assertEqual(DecodeResult#de_TestReq.age,10),
    ?debugFmt("get decode info ~p",[DecodeResult]),
    ok.
