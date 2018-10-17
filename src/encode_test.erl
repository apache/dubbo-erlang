%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2016 上午10:46
%%%-------------------------------------------------------------------
-module(encode_test).
-author("dlive").

-include("hessian.hrl").
%% API
-export([object_test/0,de_object/0]).

-record(de_TestReq, {name, nick,age}).
-record(de_reg2, {reqinfo,age}).

object_test()->
    ForeignTypeA = <<"com.ifcoder.demo.bean.UserInfoRequest">>,
    TypeDefA = #type_def{foreign_type = ForeignTypeA,
        native_type = de_TestReq,
        fieldnames = record_info(fields,de_TestReq)},
    EncodingState0 = type_encoding:enlist(TypeDefA),
    io:format("state:~p~n",[EncodingState0]),
    RequestArg0 = #de_TestReq{name = <<"nameinfo">>, nick = <<"nickname">> ,age=10 },
%%    Object = #object{values = [RequestArg0] },
%%    io:format("Object:~p~n",[Object]),
    {Bin, State0} = hessianEncode:encode(RequestArg0, EncodingState0),
    file:write_file("/tmp/object_erl.data",Bin),
%%    ?assertMatch(Expected, Bin),
    %% This test is asymmetric because decode($O,Rest/bin)
    %% will only consume the outer class definition,
    %% in normal circumstances, the calling decode/2 function will recursively
    %% consume all object definitions.
%%    {Rest, TypeDef, State1} = hessian:decode(Bin, DecodingState),
%%    ?assertMatch(TypeDefA, TypeDef),
%%    [Function|Arguments] = hessian:decode(<<99,2,0,109,0,1,97,Bin/binary>>, DecodingState),
%%    ?assertMatch([[A]], Arguments).

    ok.

de_object()->
    ForeignTypeA = <<"com.ifcoder.demo.bean.UserInfoRequest">>,
    TypeDefA = #type_def{foreign_type = ForeignTypeA,
        native_type = de_TestReq,
        fieldnames = record_info(fields,de_TestReq)},
    EncodingState0 = hessianDecode2:enlist(TypeDefA),
    {ok,Bin} = file:read_file("/tmp/object_erl.data"), %% /tmp/objectencode.data
    Info = hessianDecode2:decode(Bin,EncodingState0),
    io:format("info:~p~n",[Info]),
    ok.

