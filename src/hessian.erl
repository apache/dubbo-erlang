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

-module(hessian).

-include("hessian.hrl").

-export([call/5]).
-export([invoke/4]).
-export([decode/2]).
-export([encode/2, encode/3, encode/4, encode/5]).
%% TODO Move the e2cc function to encoding module
-export([erlang_to_camel_case/1]).

%---------------------------------------------------------------------------
% Decoding
%---------------------------------------------------------------------------

% Binaries
decode(<<16#20,Rest/binary>>, State) -> {Rest, <<>>, State};
decode(<<Len:8/unsigned,Rest/binary>>, State) when Len =< 16#2f, 16#20 < Len ->
    _Len = Len - 16#20,
    <<Bin:_Len/binary,_Rest/binary>> = Rest,
    {_Rest, Bin, State};
decode(<<$B,Len:16/unsigned,Bin:Len/binary,Rest/binary>>, State) -> {Rest, Bin, State};
decode(<<$b,Rest/binary>>,State) -> decode(<<$b,Rest/binary>>, [], State);
%% Booleans
decode(<<$T,Rest/binary>>, State) -> {Rest, true, State};
decode(<<$F,Rest/binary>>, State) -> {Rest, false, State};
%% Dates
decode(<<$d,Date:64/unsigned,Rest/binary>>, State) ->
    MegaSecs = Date div ?MegaSeconds,
    Secs = (Date - MegaSecs * ?MegaSeconds) div ?Seconds,
    MicroSecs = (Date - MegaSecs * ?MegaSeconds - Secs * ?Seconds) * ?MicroSeconds,
    {Rest, {MegaSecs, Secs, MicroSecs}, State};
%% Doubles
decode(<<16#67,Rest/binary>>, State)-> {Rest, 0.0, State};
decode(<<16#68,Rest/binary>>, State)-> {Rest, 1.0, State};
decode(<<16#69,Int:8/signed,Rest/binary>>, State)-> {Rest, float(Int), State};
decode(<<16#6a,Int:16/signed,Rest/binary>>, State)-> {Rest, float(Int), State};
%% TODO ask erlang-questions about not being able to match a 32bit float
decode(<<16#6b,Int:32/signed,Rest/binary>>, State)->
    <<Double:64/float>> = <<Int:32,0,0,0,0>>,
    {Rest, Double, State};
decode(<<$D,Double:64/float,Rest/binary>>, State)-> {Rest, Double, State};
%% Ints
decode(<<$I,Int:32/unsigned,Rest/binary>>, State)-> {Rest, Int, State};
decode(<<Int:8,Rest/binary>>, State) when Int >= 16#80, Int =< 16#bf -> {Rest, Int - 16#90, State};
decode(<<B2:8,B1:8,B0:8,Rest/binary>>, State) when B2 >= 16#d0, B2 =< 16#d7 -> {Rest, ((B2 - 16#d4) bsl 16) + (B1 bsl 8) + B0, State};
decode(<<B1:8,B0:8,Rest/binary>>, State) when B1 >= 16#c0, B1 =< 16#cf -> {Rest, ((B1 - 16#c8) bsl 8) + B0, State};
%% Longs
decode(<<$L,Long:64/unsigned,Rest/binary>>, State)-> {Rest, Long, State};
decode(<<16#77,Long:32,Rest/binary>>, State) -> {Rest, Long, State};
decode(<<Long:8,Rest/binary>>, State) when Long >= 16#d8, Long =< 16#ef -> {Rest, Long - 16#e0, State};
decode(<<B2:8,B1:8,B0:8,Rest/binary>>, State) when B2 >= 16#38, B2 =< 16#3f -> {Rest, ((B2 - 16#3c) bsl 16) + (B1 bsl 8) + B0, State};
decode(<<B1:8,B0:8,Rest/binary>>, State) when B1 >= 16#f0, B1 =< 16#ff -> {Rest, ((B1 - 16#f8) bsl 8) + B0, State};
%% Strings
decode(<<0,Rest/binary>>, State) -> {Rest, <<>>, State};
decode(<<Len:8,String:Len/binary,Rest/binary>>, State) when Len < 32 -> {Rest, list_to_binary(xmerl_ucs:from_utf8(String)), State};
decode(<<$S,Len:16/unsigned,String:Len/binary,Rest/binary>>, State) -> {Rest, list_to_binary(xmerl_ucs:from_utf8(String)), State};
decode(<<$s,Rest/binary>>, State) -> decode(<<$s,Rest/binary>>,[], State);
%% Nulls
decode(<<$N,Rest/binary>>, State) -> {Rest, undefined, State};
%% References
decode(<<$R,Ref:32/unsigned,Rest/binary>>, State)-> {ref, Rest, Ref, State};
%% Maps
decode(<<$M,$t,L:16/unsigned,Type:L/binary,Map/binary>>, State) -> decode(map, Map, dict:store(fqn, Type, dict:new()), State);
decode(<<$M,Map/binary>>, State) -> decode(map, Map, dict:new(), State);
%%list ::= V type? length? value* z
%%     ::= v int int value*
%%length     ::= 'l' b3 b2 b1 b0
%%           ::= x6e int
%% Lists
decode(<<$V,$t,L1:16/unsigned,T:L1/binary,$l,L2:32/unsigned,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$V,$l,L1:32/unsigned,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$V,16#6e,L1:8,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$V,16#6e,L1:16,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$V,$t,L1:16/unsigned,T:L1/binary,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$V,Bin/binary>>, State) -> decode(list, Bin, [], State);
decode(<<$O,?M,?m,Hash:32/unsigned,$u,$z>>, State0) -> {peer_cannot_resolve,Hash};
decode(<<$O,Hash:32/unsigned,$t,L:16/unsigned,Type:L/binary,Bin/binary>>, State0) ->
    {Rest, TypeDef, State} = decode(type_definition,Type,Bin,State0),
    NewState = type_decoding:hash_store(Hash,TypeDef,State),
    {Rest, {Hash,TypeDef}, NewState};
decode(<<$O,Hash:32/unsigned,Bin/binary>>, State) ->
    ?TYPEDEF(Hash),
    case type_decoding:hash_lookup(Hash, State) of
        {not_found, Hash} ->
            %?LOG(Hash),
            {Bin, {not_found, Hash}, State};
        TypeDef ->
            %?LOG(TypeDef),
            NewState = type_decoding:visit(TypeDef,State),
            {Bin,TypeDef,NewState}
    end;
decode(<<$o,Bin/binary>>, State) ->
    {Rest,Ref,_State} = decode(Bin, State),
    ?INSTANCE(Ref),
    ForeignView = type_decoding:lookup_reference(Ref, _State),
    #type_def{native_type = NativeType,
              foreign_type = ForeignType,
              fieldnames = ForeignFieldNames} = ForeignView,
    NativeView = type_decoding:resolve_native_type(ForeignType,_State),
    Count = count_fields(ForeignView),
    case decode(field, Rest, Count,[], _State) of
        {not_found,Hash} ->
            {not_found,Hash};
        {_Rest,FieldValues, NewState} ->
            Object = type_decoding:project_native_view(ForeignFieldNames,FieldValues,NativeView),
            {_Rest, Object, NewState}
    end;        
%---------------------------------------------------------------------------
% Call and reply decoding
%---------------------------------------------------------------------------
decode(<<$c,?M,?m,$m,L1:16/unsigned,Function:L1/binary,Bin/binary>>, State) ->
    ?START(c, [?M,?m]),
    ?METHOD(Function),
    case decode(list, Bin,[], State) of
        {error, Encoded} ->
            {error, Encoded};
        {not_found, Hash} ->
            {not_found, Hash};    
        {Rest, Arguments, NewState} ->
            [type_decoding:camel_case_to_erlang(Function), Arguments]
    end;
decode(<<$r,?M,?m,$N,$z>>, State) -> ok;
%% TODO The string decoding in the fault decoding is a bit dodgy
%% Also, have a look at the way it is being encoded
decode(<<$r,?M,?m,$f,
         4,"code",L1:8,Code:L1/binary,
         7,"message",L2:8,Message:L2/binary,
         6,"detail",L3:8,Detail:L3/binary,$z>>, State) ->
    {error, list_to_atom(binary_to_list(Message)) };
decode(<<$r,?M,?m,Args/binary>>, State) ->
    case decode(Args,[], State) of
        {<<>>, Decoded,_State} ->
            case lists:dropwhile(fun is_type_def/1, Decoded) of
                [Value] ->
                    Value;
                [H|T] ->
                    [H|T]
            end;
        {error, Encoded} ->
            {error, Encoded}
    end;
%% Type queries
decode(<<$q,?M,?m,Hash:32/unsigned,$z>>, State) ->
    ?START(q, [?M,?m]),
    ?VALUE(<<Hash:32>>),
    case type_decoding:hash_lookup(Hash, State) of
        {not_found, Hash} ->
            {error, encode(fault, <<"ProtocolException">>, unknown_hash, Hash, State) };
        TypeDef ->
            {Hash,TypeDef, State}
    end;
decode(<<$y,?M,?m,Hash:32/unsigned,$z>>, State) ->
    {type_query_ack, Hash};
%% Anything else
decode(<<Unexpected/binary>>, State) ->
    {error, encode(fault, <<"ProtocolException">>, unexpected_byte_sequence, Unexpected, State) }.
%----------------------------------
decode(<<$b,Len:16/unsigned,Bin:Len/binary,$b,Rest/binary>>, Acc, State) ->
    decode(<<$b,Rest/binary>>,Acc ++ [Bin], State);
decode(<<$b,Len:16/unsigned,Bin:Len/binary,$B,Rest/binary>>, Acc, State) ->
    _Acc = Acc ++ [Bin],
    {_Rest,_Bin, State} = decode(<<$B,Rest/binary>>, State),
    {_Rest, list_to_binary(_Acc ++ [_Bin]), State};
decode(<<$s,Len:16,String:Len/binary,$s,Rest/binary>>,Acc, State) ->
    _String = list_to_binary(xmerl_ucs:from_utf8(String)),
    decode(<<$s,Rest/binary>>,Acc ++ [_String], State);
decode(<<$s,Len:16,String:Len/binary,$S,Rest/binary>>,Acc, State) ->
    _String = list_to_binary(xmerl_ucs:from_utf8(String)),
    _Acc = Acc ++ [_String],
    {_Rest,_Bin, _State} = decode(<<$S,Rest/binary>>, State),
    {_Rest, list_to_binary(_Acc ++ [_Bin]), _State};
decode(<<>>, List, State) -> {<<>>, List, State};
decode(<<$z>>, List, State) -> {<<>>, List, State};
decode(Args, List, State) ->
    case decode(Args,State) of
        {Rest, [H|T], _State} ->
            decode(Rest, List ++ [H|T], _State);
        {Rest, Result, _State} ->
            decode(Rest, List ++ [Result], _State);
        {ref, Rest, Ref, _State} ->
            _Ref = lists:nth(Ref + 1, List),
            decode(Rest, [List,[_Ref]] , _State);
        {error, Encoded} ->
            {error, Encoded}
    end.
decode(map, <<$z>>, Dict, State) -> {Dict, State};
decode(map, <<$z,Rest/binary>>, Dict, State) -> {Rest,Dict, State};
decode(map, Bin, Dict, State) ->
    {_Rest, Key, _State} = decode(Bin, State),
    case decode(_Rest, _State) of
        {Rest, Value, __State} ->
            decode(map, Rest, dict:store(Key, Value, Dict), __State);
        {ref, Rest, Ref, __State} ->
            %Value = lists:nth(Ref + 1, List),
            Value = Ref,
            decode(map, Rest, dict:store(Key, Value, Dict), __State)
    end;
decode(list, <<>>, List, State) -> {<<>>,List, State};
decode(list, <<$z>>, List, State) -> {<<>>,List, State};
decode(list, <<$z,Rest/binary>>, List, State) -> {Rest, List, State};
decode(list, Bin, List, State) ->
    case decode(Bin, State) of
        {error, Encoded} ->
            {error, Encoded};
        {Rest, {type_def,_,_,_}, _State} ->
            decode(list, Rest, List, _State);
        {not_found,Hash} ->
            {not_found, Hash};    
        {Rest, {not_found, Hash}, _State} ->
            {not_found, Hash};    
        {Rest, Value, _State} ->
            ?VALUE(Value),
            decode(list, Rest, List ++ [Value], _State)
    end;
decode(type_definition, ForeignType, Bin, State) ->
    {Rest,Count, _State} = decode(Bin, State),
    {NewRest,FieldNames, NewState} = decode(field, Rest, Count, [], _State),
    TypeDef = type_decoding:build_foreign_view(ForeignType,FieldNames,NewState),
    {NewRest, TypeDef, NewState}.
decode(field, <<$z,Rest/binary>>, Count, Acc, State) -> {Rest, Acc, State};
decode(field, Rest, 0, Acc, State) -> {Rest, Acc, State};
decode(field, Bin, Count, Acc, State) ->
    {Rest,Field, _State} = decode(Bin, State),
    case Field of
        {not_found,Hash} ->
            {not_found,Hash};
        {type_def,_,_,_} ->
            {_Rest, Object, __State} = decode(Rest, _State),
            decode(field, _Rest, Count - 1, Acc ++ [Object], __State);
        _ ->
            decode(field, Rest, Count - 1, Acc ++ [Field], _State)
    end.

%---------------------------------------------------------------------------
% Encoding
%---------------------------------------------------------------------------

encode(type_query, Hash) -> 
    ?START(q,[?M,?m]),
    ?VALUE(<<Hash:32>>),
    <<$q,?M,?m,Hash:32/unsigned,$z>>;
encode(type_query_ok, Hash) -> <<$y,?M,?m,Hash:32/unsigned,$z>>;
encode(type_information, TypeDef = #type_def{fieldnames = FieldNames,
                                             foreign_type = ForeignType}) ->
    Size = size(ForeignType),
    Count = count_fields(TypeDef),
    Int = encode(int, Count, []),
    _FieldNames = lists:map(fun erlang_to_camel_case/1,FieldNames),
    {AccOut, _} = lists:foldl(fun encode/2,{<<>>, []},_FieldNames),
    Hash = erlang:phash2(TypeDef),
    <<$O,Hash:32/unsigned,$t,Size:16/unsigned,ForeignType/binary,Int/binary,AccOut/binary,$z>>;

encode(Value, {Acc,State}) ->
    case encode(Value, State) of
        {ValueBin, _State} ->
            {<<Acc/binary,ValueBin/binary>>,_State};
        ValueBin ->
            {<<Acc/binary,ValueBin/binary>>,State}
    end;

% Null
encode(undefined, State) -> <<$N>>;

encode(Value, State) when is_integer(Value) -> encode(int, Value, State);
encode(Value, State) when is_float(Value) -> encode(double, Value, State);
encode(Value, State) when is_boolean(Value) -> encode(boolean, Value, State);
encode(Value, State) when is_atom(Value) -> encode(string, atom_to_list(Value), State);
encode(Value, State) when is_list(Value) -> encode(list, Value, State);
encode(Value, State) when is_pid(Value) -> throw("Erlang Pid encoding not supported");
%% Assume that a binary is a string
%% TODO what about encapsulating binary-data????
encode(Value, State) when is_binary(Value) -> encode(string, Value, State);
%% TODO The order of this is_tuple guard worries me a bit, needs further attention.
encode(Value, State) when is_tuple(Value) -> encode(object, Value, State).

% x20               # zero-length binary data
% x23 x01 x02 x03   # 3 octet data
% B x10 x00 ....    # 4k final chunk of data
% b x04 x00 ....    # 1k non-final chunk of data
encode(binary, <<>>, State) -> <<16#20>>;
encode(binary, Value, State) when size(Value) < 15 ->
    Size = 16#20 + size(Value),
    <<Size:8/unsigned,Value/binary>>;
encode(binary, Value, State) -> encode(binary, Value, <<>>, State);
encode(boolean, true, State) -> <<$T>>;
encode(boolean, false, State) -> <<$F>>;
encode(timestamp, {MegaSecs, Secs, MicroSecs}, State) ->
    Date = MegaSecs * ?MegaSeconds + Secs * ?Seconds + MicroSecs div ?MicroSeconds,
    <<$d,Date:64/unsigned>>;
encode(localtime, DateTime={{Year,Month,Day},{Hour,Min,Sec}}, State) ->
    [Universal] = calendar:local_time_to_universal_time_dst(DateTime),
    Seconds = calendar:datetime_to_gregorian_seconds(Universal),
    MilliSeconds = (Seconds - ?UnixEpoch) * 1000,
    <<$d,MilliSeconds:64/unsigned>>;
encode(double, 0.0, State) -> <<16#67>>;
encode(double, 1.0, State) -> <<16#68>>;
encode(double, Double, State) when Double >= -128.0, Double =< 127.0, Double == round(Double) ->
    Byte = round(Double),
    <<16#69, Byte/signed>>;
encode(double, Double, State) when Double >= -32768.0, Double =< 32767.0, Double == round(Double) ->
    Byte = round(Double),
    <<16#6a, Byte:16/signed>>;
encode(double, Double, State) ->
    case <<Double/float>> of
        <<B24,B16,B8,B0,0,0,0,0>> ->
            <<16#6b,B24,B16,B8,B0>>;
        Other ->
            <<$D,Other/binary>>
    end;
encode(int, Int, State) when Int >= -16, Int =< 47 ->
    _Int = Int + 16#90,
    <<_Int:8>>;
encode(int, Int, State) when Int >= -2048, Int =< 2047 ->
    <<B1:8,B0:8>> = <<Int:16>>,
    _B1 = B1 + 16#c8,
    <<_B1,B0>>;
encode(int, Int, State) when Int >= -262144, Int =< 262143 ->
    <<B2:8,B1:8,B0:8>> = <<Int:24>>,
    _B2 = B2 + 16#d4,
    <<_B2,B1,B0>>;
encode(int, Int, State) when Int > 16#100000000 -> <<$L,Int:64/unsigned>>;
encode(int, Int, State) -> <<$I,Int:32/unsigned>>;
encode(long, Long, State) when Long >= -8, Long =< 15 ->
    _Long = Long + 16#e0,
    <<_Long:8>>;
encode(long, Long, State) when Long >= -2048, Long =< 2047 ->
    <<B1:8,B0:8>> = <<Long:16>>,
    _B1 = B1 + 16#f8,
    <<_B1,B0>>;
encode(long, Long, State) when Long >= -262144, Long =< 262143 ->
    <<B2:8,B1:8,B0:8>> = <<Long:24>>,
    _B2 = B2 + 16#3c,
    <<_B2,B1,B0>>;
encode(long, Long, State) when Long >= -16#100000000, Long =< 16#100000000 -> <<16#77,Long:32>>;
encode(long, Long, State) -> <<$L,Long:64/unsigned>>;
encode(string, <<>>, State) -> <<0>>;
encode(string, [], State) -> <<0>>;
encode(string, String, State) when is_binary(String)-> encode(string, binary_to_list(String), State);
encode(string, String, State) when is_list(String)->
    UTF8 = case catch xmerl_ucs:is_incharset(String,'utf-8') of
               true ->
                   String;
               _ ->
                   xmerl_ucs:to_utf8(String)
           end,
    %% There is a question pending on the hessian list as to whether the length
    %% refers to the UTF-8 or the native length
    %Length = length(String),
    Length = length(UTF8),
    if
        Length < 32 ->
            Bin = list_to_binary(UTF8),
            <<Length:8,Bin/binary>>;
        true ->
            encode(string, list_to_binary(String), <<>>, State)
    end;
encode(dictionary, Dict, State) ->
    Encoder = fun(Key, Value, AccIn) ->
                KeyBin = encode(Key, State),
                ValueBin = encode(Value, State),
                <<AccIn/binary,KeyBin/binary,ValueBin/binary>> end,
    AccOut = dict:fold(Encoder, <<$M>>, Dict),
    <<AccOut/binary,$z>>;
%% Length
%::= 'l' b3 b2 b1 b0
%::= x6e int
encode(length, List, State) ->
    Length = length(List),
    case encode(int, Length, State) of
        <<$I,Int/binary>> ->
            <<$l,Int/binary>>;
        Other ->
            <<16#6e,Other/binary>>
    end;
%% Lists
encode(list, List, State) ->
    Length = encode(length, List, State),
    encode(list, List, <<$V,Length/binary>>, State);
encode(method, Method, State) when is_atom(Method) ->
    String = atom_to_list(Method),
    encode(method, String, State);
encode(method, Method, State) when is_binary(Method) ->
    _Method = erlang_to_camel_case(Method),
    Size = size(_Method),
    <<$m,Size:16/unsigned,_Method/binary>>;
%---------------------------------------------------
% TODO I don't think this ever gets called
encode(method, String, State) when is_list(String) ->
    _String = erlang_to_camel_case(String),
    Length = string:len(_String),
    Bin = list_to_binary(_String),
    <<$m,Length:16/unsigned,Bin/binary>>;
%---------------------------------------------------
encode(type, FullyQualifiedName, State) when is_list(FullyQualifiedName) ->
    Bin = list_to_binary(FullyQualifiedName),
    encode(type,Bin, State);
encode(type, FullyQualifiedName, State) when is_binary(FullyQualifiedName) ->
    Length = size(FullyQualifiedName),
    <<$t,Length:16,FullyQualifiedName/binary>>;
%% TODO implement header
%% reply   ::= r x01 x00 header* object z
%%         ::= r x01 x00 header* fault z
encode(reply, ok, State) -> <<$r,?M,?m,$N,$z>>;
encode(reply, {ok, Object}, State) -> encode(reply, Object, State);
encode(reply, {error, {Error, Reason} }, State) -> encode(fault, Error, Reason, State);
encode(reply, Object, State) ->
    ?START(r,[?M,?m]),
    Bin = case encode(Object, State) of
              {_Bin, NewState} ->
                    _Bin;
              _Bin ->
                  _Bin
          end,
    <<$r,?M,?m,Bin/binary,$z>>;
encode(object, DateTime = {MegaSecs, Secs, MicroSecs}, State) 
    when is_integer(MegaSecs), is_integer(Secs), is_integer(MicroSecs)->
    ?LOG("Employing date heuristic"),
    encode(timestamp,DateTime,State);    
encode(object, Object, State) when is_tuple(Object) ->
    [NativeType|Values] = tuple_to_list(Object),
    {TypeEncoding, EncodedRef, NewState} =
        case type_encoding:visit(NativeType,State) of
            {ref, Ref} ->
                encode(type_information, {ref, Ref}, State);
            {hash, Hash, Ref, State0} ->
                encode(type_information, {hash, Hash, Ref}, State0)
        end,        
    {AccOut, _NewState} = lists:foldl(fun encode/2,{<<>>, NewState},Values),
    {<<TypeEncoding/binary,$o,EncodedRef/binary,AccOut/binary>>, _NewState};
encode(type_information, {ref, Ref}, State) ->
    {<<>>, encode(int, Ref, State), State};
encode(type_information, {hash, Hash, Ref}, State) ->
    EncodedRef = encode(int, Ref, State),
    {<<$O,Hash:32/unsigned>>, EncodedRef, State}.
encode(binary, Value, <<>>, State) when size(Value) =< ?CHUNK_SIZE ->
    Size = size(Value),
    <<$B,Size:16,Value/binary>>;
encode(binary, Value, Acc, State) when size(Value) =< ?CHUNK_SIZE ->
    Size = size(Value),
    <<Acc/binary,$B,Size:16,Value/binary>>;
encode(binary, Value, <<>>, State) ->
    <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
    encode(binary, Rest, <<$b,?CHUNK_SIZE:16,Chunk/binary>>, State);
encode(binary, Value, Acc, State) ->
    <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
    encode(binary, Rest, <<Acc/binary,$b,?CHUNK_SIZE:16,Chunk/binary>>, State);
encode(string, Value, Acc, State) when size(Value) =< ?CHUNK_SIZE ->
    Size = size(Value),
    <<Acc/binary,$S,Size:16,Value/binary>>;
encode(string, Value, <<>>, State) ->
    <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
    encode(string, Rest, <<$s,?CHUNK_SIZE:16,Chunk/binary>>, State);
encode(string, Value, Acc, State) ->
    <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
    encode(string, Rest, <<Acc/binary,$s,?CHUNK_SIZE:16,Chunk/binary>>, State);
%---------------------------------------------------------------------------
% List Encoding
%---------------------------------------------------------------------------
encode(list, Type, List, State) when is_binary(Type) ->
    TypeLength = size(Type),
    ListLength = length(List),
    encode(list, List, <<$V,$t,TypeLength:16/unsigned,Type/binary,$l,ListLength:32/unsigned>>, State);
encode(list, List, Acc0, State) when is_binary(Acc0) ->
    {AccOut, State} = lists:foldl(fun encode_accumulate/2, {Acc0,State}, List),
    <<AccOut/binary,$z>>;
%---------------------------------------------------------------------------
% Call and Reply Encoding
%---------------------------------------------------------------------------
%% TODO implement header call    ::= c x01 x00 header* method object* z
encode(call, Method, Args, State) ->
    encode(call, Method, Args, fun encode_accumulate/2, State);
encode(fault, _Error, _Reason, State) ->
    encode(fault, <<"ServiceException">>, _Error, _Reason, State).
encode(fault, Code, _Error, _Reason, State) ->
    EncodedCode = encode(string,Code, State),
    <<131,100,L2:16/unsigned,Error/binary>> = term_to_binary(_Error),
    EncodedError = encode(string,Error, State),
    <<$r,?M,?m,$f,
      4,"code",EncodedCode/binary,
      7,"message",EncodedError/binary,
      6,"detail",31,"Stack trace not yet implemented",
      $z>>;
encode(call, Method, Args, Fun, State) when is_function(Fun) ->
    MethodBin = encode(method,Method, State),
    {Bin, NewState} = lists:foldl(Fun, {<<>>, State}, Args),
    <<$c,?M,?m,MethodBin/binary,Bin/binary,$z>>.

%---------------------------------------------------------------------------
% Invocation
%---------------------------------------------------------------------------

invoke(Module,Bin,DecodingState,EncodingState) ->
    case decode(Bin,DecodingState) of 
        {error, Reason} ->
            encode(fault, decoding_error, Reason, EncodingState);
        {not_found, Hash} ->
            encode(type_query,Hash);
        {peer_cannot_resolve,Hash} ->
            %% TODO what should happen here? Is this good enough?
            encode(reply,ok,[]);    
        {type_query_ack, Hash} ->
            %% TODO This has not been implemented yet
            ok;    
        %% Don't know if I like this weakly typed differentiation,
        %% it seems a bit brittle    
        {_,{Hash,TypeDef},_} ->
            encode(type_query_ok, Hash);
        %% TODO look into whether this 2nd branch for type_query_ok is needed,
        %% or whether to refactor the corresponding return value from the
        %% decode function
        {_,TypeDef,_} ->
            encode(type_information, TypeDef);        
        [Function,Args] ->
            Result = apply(Module,Function,Args),
            encode(reply,Result,EncodingState)
    end.

%% This is used for synchronous transports that can respond
%% to a type query    
call(Dispatcher,Method,Args,DecodingState,EncodingState) ->
    Bin = encode(call,Method,Args,EncodingState),
    Response0 = apply(Dispatcher,[Bin]),
    case decode(Response0,DecodingState) of
        {Hash,TypeDef,_} ->
            ok = send_type_negotiation(Dispatcher,TypeDef,DecodingState),
            Response = apply(Dispatcher,[Bin]),
            decode(Response,DecodingState);    
        Other ->
            Other             
    end.
    
send_type_negotiation(Dispatcher,TypeDef,DecodingState) ->
    TypeDefBin = hessian:encode(type_information, TypeDef),
    Response = apply(Dispatcher,[TypeDefBin]),
    case hessian:decode(Response,DecodingState) of
        ok ->
            ok;    
        Error ->
            throw({type_negotiation_error,Error})             
    end.    
        
%---------------------------------------------------------------------------
% Utility methods
%---------------------------------------------------------------------------
erlang_to_camel_case(String) when is_binary(String) ->
    AsList = binary_to_list(String),
    AsCamel = lists:foldl(fun camelize/2,[],AsList),
    list_to_binary(AsCamel);
erlang_to_camel_case(String) when is_atom(String) ->
    AsList = atom_to_list(String),
    AsCamel = lists:foldl(fun camelize/2,[],AsList),
    list_to_binary(AsCamel).    
        
camelize(Element,Acc) when Element == $_ -> [$_|Acc];
camelize(Element,[$_|Acc]) -> lists:append(Acc,[Element - 16#20]);
camelize(Element,Acc) -> lists:append(Acc,[Element]).
    

%% May want to take another look at this:
%% All of the primitive encoding functions do not return the state.
%% Maybe they should to avoid differentiations like this.
encode_accumulate(Value, {Acc, State}) ->
    case encode(Value, State) of 
        {Encoded,NewState} ->
             {<<Acc/binary,Encoded/binary>>,NewState};
        Encoded ->
            {<<Acc/binary,Encoded/binary>>,State}
    end.

is_type_def(#type_def{}) -> true;
is_type_def(_) -> false.

count_fields(#type_def{fieldnames = FieldNames}) -> length(FieldNames).
