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

-module(cotton_hessian).

-include("hessian.hrl").

-export([encode/2, encode/3, encode/4, encode/5]).

-export([decode/2]).
-export([init/0]).
-export([get_deftype/2]).
-record(decoding_state,{type_pool = dict:new(), reference_pool = dict:new(),hash_pool = dict:new()}).

%---------------------------------------------------------------------------
% encode/2
%---------------------------------------------------------------------------

encode(undefined, _State) -> <<$N>>;
encode(null, _State) -> <<$N>>;
encode(true, _State) -> <<$T>>;
encode(false, _State) -> <<$F>>;
encode(Value, State) when is_integer(Value) -> encode(int, Value, State);
encode(Value, State) when is_atom(Value) -> encode(string, erlang:atom_to_binary(Value,utf8), State);
encode(Value, State) when is_float(Value) -> encode(double, Value, State);
encode(Value, State) when is_list(Value) -> encode(string, Value, State);
encode(Value, State) when is_binary(Value) -> encode(string, Value, State);
encode(Value, State) when is_tuple(Value) -> encode(struct, Value, State).
%---------------------------------------------------------------------------
% encode/3
%---------------------------------------------------------------------------
encode(struct, Input, State) ->
  case Input of
    #list{} -> encode(list, Input, State);
    #map{} -> encode(map, Input, State);
    #object{} -> encode(object, Input, State);
    #class{} -> encode(class_store, Input, State);
    {binary, Bin} -> encode(binary, Bin, State);
    {date, Timestamp} -> encode(date, Timestamp, State);
    {ref, Index} ->
      IndexBin = encode(int, Index, State),
      <<$Q, IndexBin/binary>>;
    _Object ->
      logger:debug("[encode] object ~p",[Input]),
      encode(class_object, Input, State);
%%			{<<>>,State};
    {K, V} ->
      {BK,SK} = encode(K, State),
      {BV,SV} = encode(V, SK),
      {<<BK/binary, BV/binary>>, SV}
  end;
encode(int, Int, _State) when Int >= -16, Int =< 47 ->
  _Int = Int + 16#90,
  <<_Int:8>>;
encode(int, Int, _State) when Int >= -2048, Int =< 2047 ->
  <<B1:8,B0:8>> = <<Int:16>>,
  _B1 = B1 + 16#c8,
  <<_B1,B0>>;
encode(int, Int, _State) when Int >= -262144, Int =< 262143 ->
  <<B2:8,B1:8,B0:8>> = <<Int:24>>,
  _B2 = B2 + 16#d4,
  <<_B2,B1,B0>>;
encode(int, Int, _State) when Int > -16#80000001, Int < 16#80000000 ->
  <<$I,Int:32/signed>>;
encode(int, Int, State) ->
  encode(long, Int, State);
encode(long, Long, _State) when Long >= -8, Long =< 15 ->
  _Long = Long + 16#e0,
  <<_Long:8>>;
encode(long, Long, _State) when Long >= -2048, Long =< 2047 ->
  <<B1:8,B0:8>> = <<Long:16>>,
  _B1 = B1 + 16#f8,
  <<_B1,B0>>;
encode(long, Long, _State) when Long >= -262144, Long =< 262143 ->
  <<B2:8,B1:8,B0:8>> = <<Long:24>>,
  _B2 = B2 + 16#3c,
  <<_B2,B1,B0>>;
encode(long, Long, _State) when Long > -16#80000001, Long < 16#80000000 ->
  <<16#59,Long:32/signed>>;
encode(long, Long, _State) ->
  <<$L,Long:64/signed>>;
encode(double, 0.0, _State) ->
  <<16#5b>>;
encode(double, 1.0, _State) ->
  <<16#5c>>;
encode(double, Double, _State) when Double >= -128.0, Double =< 127.0, Double == round(Double) ->
  Byte = round(Double),
  <<16#5d, Byte/signed>>;
encode(double, Double, _State) when Double >= -32768.0, Double =< 32767.0, Double == round(Double) ->
  Byte = round(Double),
  <<16#5e, Byte:16/signed>>;
encode(double, Double, _State) ->
  case <<Double/float>> of
    <<B24,B16,B8,B0,0,0,0,0>> -> <<16#5f,B24,B16,B8,B0>>;
    Other -> <<$D,Other/binary>>
  end;
encode(string, <<>>, _State) ->
  <<0>>;
encode(string, [], _State) ->
  <<0>>;
encode(string, String, State) when is_list(String)->
  UTF8 = xmerl_ucs:from_utf8(String),
  Length = length(UTF8),
  encode(sub, Length, String, [UTF8|State]);
encode(string, String, State) when is_binary(String)->
  encode(string, binary_to_list(String), State);
encode(binary, <<>>, _State) ->
  <<16#20>>;
encode(binary, Value, _State) when size(Value) < 15 ->
  Size = 16#20 + size(Value),
  <<Size:8/unsigned,Value/binary>>;
encode(binary, Value, State) ->
  encode(binary, Value, <<>>, State);
encode(date, {MegaSeconds,Seconds,MicroSeconds}, State) ->
  MilliSeconds = MegaSeconds * ?MegaSeconds + Seconds * ?Seconds + MicroSeconds div ?MicroSeconds,
  encode(date, MilliSeconds, State);
encode(date, MilliSeconds, _State) ->
  MinuteRemain = MilliSeconds rem (?Seconds * 60),
  case MinuteRemain of
    0 ->
      Minutes = MilliSeconds div (?Seconds * 60),
      <<16#4b,Minutes:32/unsigned>>;
    _ -> <<16#4a,MilliSeconds:64/unsigned>>
  end;
encode(list, Input, [{set,SetInfo},_T]=State) when is_record(Input, list) ->
  case find_set_info(Input#list.refNo, SetInfo) of
    not_found -> encode(list, Input#list.len, Input#list.type, Input#list.values, State);
    List -> encode(list, List#list.len, List#list.type, List#list.values, State)
  end;
encode(list, Input, State) when is_record(Input, list) ->
  encode(list, Input#list.len, Input#list.type, Input#list.values, State);
encode(list, List, State) ->
  ListLength = length(List),
  encode(fixedlist, ListLength, List, State);
encode(vlist, List, State) ->
  case List of
    [] -> <<$W,$Z>>;
    _ -> encode(vlist, List, <<$W>>, State)
  end;
encode(map, Input, [{set,SetInfo},_T]=State) when is_record(Input, map) ->
  case find_set_info(Input#map.refNo, Input#map.type, SetInfo) of
    not_found -> encode(map, Input#map.dict, State);
    Map -> encode(map, Map#map.dict, State)
  end;
encode(map, Input, State) when is_record(Input, map) ->
  encode(map, Input#map.dict, State);
encode(map, Dict, State) ->
  List = dict:to_list(Dict),
  Encoder = fun({Key, Value}, {AccIn, StateIn}) ->
    % Key
    RK = encode(Key, StateIn),
    {KeyBin, NewStateKey} = case RK of
                              {_, _} -> RK;
                              KeyB -> {KeyB, StateIn}
                            end,
    % Value
    RV = encode(Value, NewStateKey),
    {ValueBin, NewStateValue} = case RV of
                                  {_, _} -> RV;
                                  ValueB -> {ValueB, NewStateKey}
                                end,
    % Acc Out
    {<<AccIn/binary,KeyBin/binary,ValueBin/binary>>, NewStateValue}
            end,
  {AccOut, NewState} = lists:foldl(Encoder, {<<$H>>, State}, List),
  {<<AccOut/binary,$Z>>, NewState};
encode(class, #class{name=Name, fields=Fields}, State) ->
  % type
  NameBin = encode(Name, State),
  % fields length
  FieldsLen = length(Fields),
  FieldsLenBin = encode(int, FieldsLen, State),
  % fields
  encode(list, Fields, <<$C,NameBin/binary,FieldsLenBin/binary>>, State);
encode(class_store, Input, [{set,SetInfo},ClassList]=State) ->
  {Bin, _} = encode(class, Input, State),
  {Bin, [{set,SetInfo},[Input#class{encoded=true}|ClassList]]};
encode(class_store, Input, State) ->
  {Bin, _} = encode(class, Input, State),
%%	{Bin, [Input#class{encoded=true}|State]};
  {Bin, State};
encode(object, Input, [{set,SetInfo},T]=State) when is_record(Input, object) ->
  ClassRes = class(Input#object.typeRef, Input#object.class, [], T),
  {BaseBin, TypeNum, C, NewState} = case ClassRes of
                                      {encoded, Class, TypeNo} -> {<<>>, TypeNo, Class, State};
                                      {Class, TypeNo, NewST} ->
                                        {Bin, NewClassST} = encode(class, Class, NewST),
                                        {Bin, TypeNo, Class, [{set,SetInfo},NewClassST]}
                                    end,
  case find_set_info(Input#object.refNo, C#class.name, SetInfo) of
    not_found -> encode(object, BaseBin, TypeNum, Input#object.values, NewState);
    Obj -> encode(object, BaseBin, TypeNum, Obj#object.values, NewState)
  end;
encode(object, Input, State) when is_record(Input, object) ->
  ClassRes = class(Input#object.typeRef, Input#object.class, [], State),

  {BaseBin, TypeNum, NewState} = case ClassRes of
                                   {encoded, _Class, TypeNo} -> {<<>>, TypeNo, State};
                                   {Class, TypeNo, NewST} ->
                                     {Bin, NewClassST} = encode(class, Class, NewST),
                                     {Bin, TypeNo, NewClassST}
                                 end,
  encode(object, BaseBin, TypeNum, Input#object.values, NewState);
encode(class_object,Input,State)->
  [NativeType|Values] = tuple_to_list(Input),
  {ClassEncodingBin, EncodedRef, NewState} =
    case type_encoding:visit(NativeType,State) of
      {ref, Ref} ->
%%				encode_object(type_information, {ref, Ref}, State);
        %% todo need check
        {<<>>,Ref,State};
      {hash, Ref, Typedef , State0} ->
        Class = typedef_to_class(Typedef,Ref),
        {Bin,NewStateClass} = encode(class_store,Class,State0),
        {Bin,Ref,NewStateClass}
%%				encode(type_information, {hash, Ref,Typedef }, State0)
    end,
  encode(object, ClassEncodingBin, EncodedRef, Values, NewState);
%%	{AccOut, _NewState} = lists:foldl(fun encode/2,{<<>>, NewState},Values),
%%	{<<ClassEncodingBin/binary,$o,EncodedRef/binary,AccOut/binary>>, _NewState};

encode(method, Method, State) when is_atom(Method) ->
  String = atom_to_list(Method),
  encode(method, String, State);
encode(method, Method, _State) when is_binary(Method) ->
  CamMethod = erlang_to_camel_case(Method),
  Size = size(CamMethod),
  <<$m,Size:16/unsigned,CamMethod/binary>>;
encode(method, String, _State) when is_list(String) ->
  CamString = erlang_to_camel_case(String),
  Length = string:len(CamString),
  Bin = list_to_binary(CamString),
  <<$m,Length:16/unsigned,Bin/binary>>;
encode(reply, ok, _State) ->
  <<$H,16#02,16#00,$R,16#01,$N>>;
encode(reply, {ok, Object}, State) ->
  encode(reply, Object, State);
encode(reply, {error, {Error, Reason} }, State) ->
  encode(fault, Error, Reason, State);
encode(reply, Object, State) ->
  case encode(Object, State) of
    {Bin, _NewState} -> Bin;
    Bin -> Bin
  end,
  <<$H,16#02,16#00,$R,Bin/binary>>.
%---------------------------------------------------------------------------
% encode/4
%---------------------------------------------------------------------------
encode(sub, Length, String, _State) when Length < 32 ->
  Bin = list_to_binary(String),
  <<Length:8,Bin/binary>>;
encode(sub, Length, String, _State) when Length < 256 ->
  Bin = list_to_binary(String),
  <<16#30,Length:8,Bin/binary>>;
encode(sub, Length, String, _State) when Length < 512 ->
  Bin = list_to_binary(String),
  <<16#31,(Length-256):8,Bin/binary>>;
encode(sub, Length, String, _State) when Length < 768 ->
  Bin = list_to_binary(String),
  <<16#32,(Length-512):8,Bin/binary>>;
encode(sub, Length, String, _State) when Length < ?CHUNK_SIZE ->
  Bin = list_to_binary(String),
  <<16#33,(Length-768):8,Bin/binary>>;
encode(sub, Length, String, _State) when Length =:= ?CHUNK_SIZE ->
  Bin = list_to_binary(String),
  <<$S,Length:16,Bin/binary>>;
encode(sub, Length, _String, [UTF8|State]) ->
  encode(string, Length, UTF8, <<>>, State);
encode(binary, Value, <<>>, _State) when size(Value) =< ?CHUNK_SIZE ->
  Size = size(Value),
  <<$B,Size:16,Value/binary>>;
encode(binary, Value, Acc, _State) when size(Value) =< ?CHUNK_SIZE ->
  Size = size(Value),
  <<Acc/binary,$B,Size:16,Value/binary>>;
encode(binary, Value, <<>>, State) ->
  <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
  encode(binary, Rest, <<$b,?CHUNK_SIZE:16,Chunk/binary>>, State);
encode(binary, Value, Acc, State) ->
  <<Chunk:?CHUNK_SIZE/binary,Rest/binary>> = Value,
  encode(binary, Rest, <<Acc/binary,$b,?CHUNK_SIZE:16,Chunk/binary>>, State);
encode(list, List, Acc0, State) when is_binary(Acc0) ->
  lists:foldl(fun encode_accumulate/2, {Acc0,State}, List);
encode(list, Type, List, State) ->
  ListLength = length(List),
  encode(list, ListLength, Type, List, State);
encode(vlist, List, Acc0, State) when is_binary(Acc0) ->
  {AccOut, NewState} = lists:foldl(fun encode_accumulate/2, {Acc0,State}, List),
  {<<AccOut/binary,$Z>>, NewState};
encode(vlist, Type, List, State) ->
  TypeBin = encode(Type, State),
  case List of
    [] -> <<$U,TypeBin/binary,$Z>>;
    _ -> encode(vlist, List, <<$U,TypeBin/binary>>, State)
  end;
encode(fixedlist, ListLength, List, State) ->
  case ListLength < 8 of
    true ->
      ListLengthFlag = 16#78 + ListLength,
      case List of
        [] -> <<ListLengthFlag:8>>;
        _ -> encode(list, List, <<ListLengthFlag:8>>, State)
      end;
    false ->
      ListLengthBin = encode(int, ListLength, State),
      encode(list, List, <<$X,ListLengthBin/binary>>, State)
  end;
encode(fault, _Error, _Reason, State) ->
  encode(fault, <<"ServiceException">>, _Error, _Reason, State);
encode(call, Method, Args, State) ->
  encode(call, Method, Args, fun encode_accumulate/2, State).
%---------------------------------------------------------------------------
% encode/5
%---------------------------------------------------------------------------
encode(string, Length, UTF8, Acc, _State) when Length =< ?CHUNK_SIZE ->
  Bin = list_to_binary(xmerl_ucs:to_utf8(UTF8)),
  <<Acc/binary,$S,Length:16,Bin/binary>>;
encode(string, Length, UTF8, Acc, State) ->
  {Chunk, Rest} = lists:split(?CHUNK_SIZE, UTF8),
  ChunkBin = list_to_binary(xmerl_ucs:to_utf8(Chunk)),
  encode(string, (Length-?CHUNK_SIZE), Rest, <<Acc/binary,$R,?CHUNK_SIZE:16,ChunkBin/binary>>, State);
encode(list, -1, untyped, List, State) ->
  encode(vlist, List, State);
encode(list, -1, Type, List, State) ->
  encode(vlist, Type, List, State);
encode(list, Len, untyped, List, State) ->
  encode(fixedlist, Len, List, State);
encode(list, Len, Type, List, State) ->
  TypeBin = encode(Type, State),
  if
    Len < 8 ->
      ListLengthFlag = 16#70 + Len,
      case List of
        [] -> <<ListLengthFlag:8,TypeBin/binary>>;
        _ -> encode(list, List, <<ListLengthFlag:8,TypeBin/binary>>, State)
      end;
    true ->
      ListLengthBin = encode(int, Len, State),
      encode(list, List, <<$V,TypeBin/binary,ListLengthBin/binary>>, State)
  end;
encode(object, BaseBin, TypeNo, Values, State) ->
  IndexBin = if
               TypeNo < 16 ->
                 logger:debug("[encode] encode object TypeNo ~p",[TypeNo]),
                 IndexWrap = TypeNo + 16#60,
                 <<IndexWrap:8>>;
               true ->
                 IndexWrap = encode(int, TypeNo, State),
                 <<$O, IndexWrap/binary>>
             end,
  {Bin, NewState} = encode(list, Values, IndexBin, State),
  {<<BaseBin/binary,Bin/binary>>, NewState};
encode(call, Method, Args, Fun, State) when is_function(Fun) ->
  MethodBin = encode(string, Method, State),
  ArgsCount = encode(int, erlang:length(Args), State),
  {Bin, _NewState} = lists:foldl(Fun, {<<>>, State}, Args),
  <<$H,2,0,$C,MethodBin/binary,ArgsCount/binary,Bin/binary>>;
encode(fault, Code, _Error, _Reason, State) ->
  EncodedCode = encode(string,Code, State),
  <<131,100,_L2:16/unsigned,Error/binary>> = term_to_binary(_Error),
  EncodedError = encode(string,Error, State),
  <<$H,16#02,16#00,$F,$H,4,"code",EncodedCode/binary,7,"message",EncodedError/binary,6,"detail",31,"Stack trace not yet implemented",$Z>>.

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

encode_accumulate(Value, {Acc, State}) ->
  logger:debug("[encode] encode_accumulate value ~p",[Value]),
  case encode(Value, State) of
    {Encoded,NewState} -> {<<Acc/binary,Encoded/binary>>,NewState};
    Encoded -> {<<Acc/binary,Encoded/binary>>,State}
  end.

find_set_info(RefNo, SetInfo) ->
  find_set_info(RefNo, untyped, SetInfo).
find_set_info(_RefNo, _Type, []) ->
  not_found;
find_set_info(RefNo, Type, [Set|SetInfo]) ->
  if
    RefNo =:= Set#set.ref -> Set#set.value;
    true ->
      case equal(Type, Set#set.ref) of
        true -> Set#set.value;
        false -> find_set_info(RefNo, Type, SetInfo)
      end
  end.

type_index(Type, [CurrentType|RestTypes]) when is_record(Type, class) ->
  case equal(Type#class.name, CurrentType#class.name) of
    true -> length(RestTypes);
    false -> type_index(Type, RestTypes)
  end;
type_index(Type, [CurrentType|RestTypes]) ->
  case equal(Type, CurrentType#class.name) of
    true -> length(RestTypes);
    false -> type_index(Type, RestTypes)
  end.

get_type_no(C, RestClasses) when is_record(C, class) ->
  case C#class.typeNo of
    -1 -> length(RestClasses);
    _ -> C#class.typeNo
  end.

get_type(CListH, C, CListE) when is_record(C, class) ->
  TypeNo = get_type_no(C, CListE),
  case C#class.encoded of
    true -> {encoded, C, TypeNo};
    false -> {C, TypeNo, CListH++[C#class{encoded=true}|CListE]}
  end.

class(-1, auto, [], [C|CListE]) ->
  get_type([], C, CListE);
class(-1, Class, CListH, [C|CListE]) when is_record(Class, class) ->
  case Class#class.name of
    auto ->
      case CListH of
        [] -> get_type([], C, CListE);
        [H|T] ->get_type([], H, T++[C|CListE])
      end;
    _ ->
      case equal(Class#class.name, C#class.name) of
        true -> get_type(CListH, C, CListE);
        false -> class(-1, Class, CListH++[C], CListE)
      end
  end;
class(-1, Class, CListH, [C|CListE]) ->
  case equal(Class, C#class.name) of
    true -> get_type(CListH, C, CListE);
    false -> class(-1, Class, CListH++[C], CListE)
  end;
class(RefNo, _Class, CListH, [C|CListE]) ->
  if
    RefNo =:= length(CListE) -> get_type(CListH, C, CListE);
    true -> class(RefNo, _Class, CListH++[C], CListE)
  end.

equal(S1, S2) when is_binary(S1) ->
  if
    S1 =:= S2 -> true;
    true ->
      S1List = binary_to_list(S1),
      if
        S1List =:= S2 -> true;
        true -> false
      end
  end;
equal(S1, S2) when is_list(S1) ->
  if
    S1 =:= S2 -> true;
    true ->
      S1Binary = list_to_binary(S1),
      if
        S1Binary =:= S2 -> true;
        true -> false
      end
  end;
equal(_S1, _S2) ->
  false.


typedef_to_class(Typedef,DefineNo)->
  #class{typeNo=DefineNo, name=Typedef#type_def.foreign_type, fields=Typedef#type_def.fieldnames}.




%---------------------------------------------------------------------------
% Decoding
%---------------------------------------------------------------------------
decode_str(Rest, 0, R) ->
  {list_to_binary(xmerl_ucs:to_utf8(lists:reverse(R))), Rest};
decode_str(<<C/utf8, Rest/binary>>, N, R) ->
  decode_str(Rest, N-1, [C|R]).

% Call
decode(<<$H,?M,?m,$C,Bin/binary>>, State) ->
  {Rest, Function, _State} = decode(Bin,State),
  {Rest2, Count, __State} = decode(Rest,_State),
  case decode(list, Rest2,Count,[], __State) of
    {error, Encoded} ->
      {error, Encoded};
    {not_found, Hash} ->
      {not_found, Hash};
    {_Rest, Arguments, _NewState} ->
      [Function, Arguments]
  end;
% Fault
decode(<<$H,?M,?m,$F,
  Rest/binary>>, State) ->
  {_Rest,#map{dict=Dict},_State} = decode(Rest, State),
  {ok, Message} = dict:find(<<"message">>, Dict),
  {error, Message};
% Reply
decode(<<$H,?M,?m,$R,Args/binary>>, State) ->
  case decode(Args,[], State) of
    {<<>>, Decoded,_State} ->
      TypeSet = get_type_set(_State),
      case Decoded of
        [Value] ->
          {Value,TypeSet};
        [H|T] ->
          {[H|T],TypeSet}
      end;
    {error, Encoded} ->
      {error, Encoded}
  end;
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
decode(<<16#4a,Date:64/unsigned,Rest/binary>>, State) ->
  MegaSecs = Date div ?MegaSeconds,
  Secs = (Date - MegaSecs * ?MegaSeconds) div ?Seconds,
  MicroSecs = (Date - MegaSecs * ?MegaSeconds - Secs * ?Seconds) * ?MicroSeconds,
  {Rest, {MegaSecs, Secs, MicroSecs}, State};
decode(<<16#4b,Date:32/unsigned,Rest/binary>>, State) ->
  MegaSecs = (Date * 60000) div ?MegaSeconds,
  Secs = (Date * 60000 - MegaSecs * ?MegaSeconds) div ?Seconds,
  {Rest, {MegaSecs, Secs, 0}, State};
%% Doubles
decode(<<16#5b,Rest/binary>>, State)-> {Rest, 0.0, State};
decode(<<16#5c,Rest/binary>>, State)-> {Rest, 1.0, State};
decode(<<16#5d,Int:8/signed,Rest/binary>>, State)-> {Rest, float(Int), State};
decode(<<16#5e,Int:16/signed,Rest/binary>>, State)-> {Rest, float(Int), State};
decode(<<16#5f,Int:32/signed,Rest/binary>>, State)->
  <<Double:64/float>> = <<Int:32,0,0,0,0>>,
  {Rest, Double, State};
decode(<<$D,Double:64/float,Rest/binary>>, State)-> {Rest, Double, State};
%% Ints
decode(<<$I,Int:32/signed,Rest/binary>>, State)-> {Rest, Int, State};
decode(<<Int:8,Rest/binary>>, State) when Int >= 16#80, Int =< 16#bf -> {Rest, Int - 16#90, State};
decode(<<B2:8,B1:8,B0:8,Rest/binary>>, State) when B2 >= 16#d0, B2 =< 16#d7 -> {Rest, ((B2 - 16#d4) bsl 16) + (B1 bsl 8) + B0, State};
decode(<<B1:8,B0:8,Rest/binary>>, State) when B1 >= 16#c0, B1 =< 16#cf -> {Rest, ((B1 - 16#c8) bsl 8) + B0, State};
%% Longs
decode(<<$L,Long:64/signed,Rest/binary>>, State)-> {Rest, Long, State};
decode(<<16#59,Long:32/signed,Rest/binary>>, State) -> {Rest, Long, State};
decode(<<Long:8,Rest/binary>>, State) when Long >= 16#d8, Long =< 16#ef -> {Rest, Long - 16#e0, State};
decode(<<B2:8,B1:8,B0:8,Rest/binary>>, State) when B2 >= 16#38, B2 =< 16#3f -> {Rest, ((B2 - 16#3c) bsl 16) + (B1 bsl 8) + B0, State};
decode(<<B1:8,B0:8,Rest/binary>>, State) when B1 >= 16#f0, B1 =< 16#ff -> {Rest, ((B1 - 16#f8) bsl 8) + B0, State};
%% Strings
decode(<<0,Rest/binary>>, State) -> {Rest, <<>>, State};
decode(<<Len:8,Rest/binary>>, State) when Len < 32 ->
  {String, NewRest} = decode_str(Rest, Len, []),
  {NewRest, String, State};
decode(<<B1:8,B0:8,Rest/binary>>, State) when B1 =< 16#33,B1 >= 16#30 ->
  Len = ((B1 - 16#30) bsl 8) + B0,
  {String, NewRest} = decode_str(Rest, Len, []),
  {NewRest, String, State};
decode(<<$S,Len:16/unsigned,Rest/binary>>, State) ->
  {String, NewRest} = decode_str(Rest, Len, []),
  {NewRest, String, State};
decode(<<$R,Rest/binary>>, State) -> decode(<<$R,Rest/binary>>,[], State);
%% Nulls
decode(<<$N,Rest/binary>>, State) -> {Rest, undefined, State};
%% References
decode(<<$Q,Bin/binary>>, State)->
  {Rest, IntRef, _State} = decode(Bin,State),
  {Rest, {ref, IntRef}, _State};
%% Maps
decode(<<$M,Map/binary>>, State) ->
  {Rest,Type,_State} = decode(Map, State),
  {_Refnum,__State} = visit(#type_def{foreign_type = map},_State),
  {_Rest,Dict, NewState} = decode(map, Rest, dict:new(), __State),
  {_Rest,#map{refNo=_Refnum,type=Type,dict=Dict},NewState};
decode(<<$H,Map/binary>>, State) ->
  {_Refnum,_State} = visit(#type_def{foreign_type = map},State),
  {_Rest,Dict, NewState} = decode(map, Map, dict:new(), _State),
  {_Rest,#map{refNo=_Refnum,dict=Dict},NewState};
%% Lists
% 'V' type int value*   # fixed-length list
decode(<<$V,Bin/binary>>, State) ->
  {Rest1,_Type,_State1} = decode(Bin,State),
  {_Refnum,__State} = visit(#type_def{foreign_type = list},_State1),
  {Rest2,Len,_State2} = decode(Rest1,__State),
  {NewRest,List,NewState} = decode(list, Rest2, Len, [], _State2),
  {NewRest,#list{refNo=_Refnum,len=Len,type=_Type,values=List},NewState};
% 'X' int value*        # fixed-length untyped list
decode(<<$X,Bin/binary>>, State) ->
  {Rest1,Len,_State} = decode(Bin,State),
  {_Refnum,__State} = visit(#type_def{foreign_type = list},_State),
  {NewRest,List,NewState} = decode(list, Rest1, Len, [], __State),
  {NewRest,#list{refNo=_Refnum,len=Len,values=List},NewState};
% [x70-77] type value*  # fixed-length typed list
decode(<<H:5,Len:3,Bin/binary>>, State) when H == 14 ->
  {Rest1,_Type,_State} = decode(Bin,State),
  {_Refnum,__State} = visit(#type_def{foreign_type = list},_State),
  {NewRest,List,NewState} = decode(list, Rest1, Len, [], __State),
  {NewRest,#list{refNo=_Refnum,len=Len,type=_Type,values=List},NewState};
% [x78-7f] value*       # fixed-length untyped list
decode(<<H:5,Len:3,Bin/binary>>, State) when H == 15 ->
  {_Refnum,_State} = visit(#type_def{foreign_type = list},State),
  {NewRest,List,NewState} = decode(list, Bin, Len, [], _State),
  {NewRest,#list{refNo=_Refnum,len=Len,values=List},NewState};
% 'U' type value* 'Z'   # variable-length list
decode(<<$U,Bin/binary>>, State) ->
  {Rest1,_Type,_State} = decode(Bin,State),
  {_Refnum,__State} = visit(#type_def{foreign_type = list},_State),
  {NewRest,List,NewState} = decode(list, Rest1, [], __State),
  {NewRest,#list{refNo=_Refnum,type=_Type,values=List},NewState};
% 'W' value* 'Z'        # variable-length untyped list
decode(<<$W,Bin/binary>>, State) ->
  {_Refnum,_State} = visit(#type_def{foreign_type = list},State),
  {NewRest,List,NewState} = decode(list, Bin, [], _State),
  {NewRest,#list{refNo=_Refnum,values=List},NewState};
decode(<<$C,Bin/binary>>, State0) ->
  {Rest1,Type,State1} = decode(Bin,State0),
  {Rest, TypeDef, State} = decode(type_definition,Type,Rest1,State1),
  NewState = hash_store(TypeDef,State),
  decode(Rest,NewState);
decode(<<$O,Bin/binary>>, State) ->
  {Rest,Ref,_State} = decode(Bin, State),
  case hash_lookup(Ref, _State) of
    {not_found, Ref} ->
      {Rest, {not_found, Ref}, _State};
    ForeignView ->
      #type_def{fieldnames = ForeignFieldNames} = ForeignView,
      {_Refnum,__State} = visit(ForeignView,_State),
      Count = count_fields(ForeignView),
      case decode(field, Rest, Count,[], __State) of
        {not_found,Hash} ->
          {not_found,Hash};
        {_Rest,FieldValues, NewState} ->
          Object = project_native_view(ForeignFieldNames,FieldValues,ForeignView),
          {_Rest, #object{refNo=_Refnum, typeRef=Ref,values=Object}, NewState}
      end
  end;
decode(<<H:4,Ref:4,Rest/binary>>, _State) when H == 6 ->
  logger:debug("decode data find ref ~p",[Ref]),
  case hash_lookup(Ref, _State) of
    {not_found, Ref} ->
      {Rest, {not_found, Ref}, _State};
    ForeignView ->
      #type_def{fieldnames = ForeignFieldNames} = ForeignView,
      {_Refnum,__State} = visit(ForeignView,_State),
      Count = count_fields(ForeignView),
      case decode(field, Rest, Count,[], __State) of
        {not_found,Hash} ->
          {not_found,Hash};
        {_Rest,FieldValues, NewState} ->
          Object = project_native_view(ForeignFieldNames,FieldValues,ForeignView),
          {_Rest, #object{refNo=_Refnum, typeRef=Ref,values=Object}, NewState}
      end
  end;
%% Anything else
decode(<<Unexpected/binary>>, State) ->
  {error, cotton_hessian:encode(fault, <<"ProtocolException">>, unexpected_byte_sequence, Unexpected, State) }.
decode(<<$b,Len:16/unsigned,Bin:Len/binary,$b,Rest/binary>>, Acc, State) ->
  decode(<<$b,Rest/binary>>,Acc ++ [Bin], State);
decode(<<$b,Len:16/unsigned,Bin:Len/binary,$B,Rest/binary>>, Acc, State) ->
  _Acc = Acc ++ [Bin],
  {_Rest,_Bin, State} = decode(<<$B,Rest/binary>>, State),
  {_Rest, list_to_binary(_Acc ++ [_Bin]), State};
decode(<<$R,Len:16/unsigned,Rest/binary>>,Acc, State) ->
  {_String, NewRest} = decode_str(Rest, Len, []),
  <<H:8,_/binary>> = NewRest,
  case H of
    $R -> decode(NewRest,list_to_binary([Acc|[_String]]), State);
    $S -> {_Rest,_Bin, _State} = decode(NewRest, State),
      _Acc2 =[Acc |[_String]],
      {_Rest, list_to_binary(_Acc2 ++ [_Bin]), _State}
  end;
decode(<<>>, List, State) -> {<<>>, List, State};
decode(<<$Z>>, List, State) -> {<<>>, List, State};
decode(Args, List, State) ->
  case decode(Args,State) of
    {Rest,{ref, Ref}, _State} ->
      decode(Rest, List ++ [{ref, Ref}] , _State);
    {Rest, [H|T], _State} ->
      decode(Rest, List ++ [H|T], _State);
    {Rest, Result, _State} ->
      decode(Rest, List ++ [Result], _State);
    {error, Encoded} ->
      {error, Encoded}
  end.
decode(map, <<$Z>>, Dict, State) -> {<<>>,Dict, State};
decode(map, <<$Z,Rest/binary>>, Dict, State) -> {Rest,Dict, State};
decode(map, Bin, Dict, State) ->
  {_Rest, Key, _State} = decode(Bin, State),
  case decode(_Rest, _State) of
    {Rest, {ref, Ref}, __State} ->
      %Value = lists:nth(Ref + 1, List),
      Value = Ref,
      decode(map, Rest, dict:store(Key, {ref, Value}, Dict), __State);
    {Rest, Value, __State} ->
      decode(map, Rest, dict:store(Key, Value, Dict), __State)
  end;
decode(list, <<>>, List, State) -> {<<>>,lists:reverse(List), State};
decode(list, <<$Z>>, List, State) -> {<<>>,lists:reverse(List), State};
decode(list, <<$Z,Rest/binary>>, List, State) -> {Rest, lists:reverse(List), State};
decode(list, Bin, List, State) ->
  case decode(Bin, State) of
    {error, Encoded} ->
      {error, Encoded};
    {not_found,Hash} ->
      {not_found, Hash};
    {_Rest, {not_found, Hash}, _State} ->
      {not_found, Hash};
    {Rest, Value, _State} ->
      decode(list, Rest, [Value|List], _State)
  end;
decode(type_definition, ForeignType, Bin, State) ->
  {Rest,Count, _State} = decode(Bin, State),
  {NewRest,FieldNames, NewState} = decode(field, Rest, Count, [], _State),
  {TypeDef,NewState2} = build_foreign_view(ForeignType,FieldNames,NewState),
  {NewRest, TypeDef, NewState2}.
decode(list, Bin, 0, List, State) -> {Bin, lists:reverse(List), State};
decode(list, Bin, Len, List, State) ->
  case decode(Bin, State) of
    {error, Encoded} ->
      {error, Encoded};
    {not_found,Hash} ->
      {not_found, Hash};
    {_Rest, {not_found, Hash}, _State} ->
      {not_found, Hash};
    {Rest, Value, _State} ->
      decode(list, Rest, Len - 1, [Value|List], _State)
  end;
decode(field, Rest, 0, Acc, State) -> {Rest, Acc, State};
decode(field, <<$Z,Rest/binary>>, _Count, Acc, State) -> {Rest, Acc, State};
decode(field, Bin, Count, Acc, State) ->
  {Rest,Field, _State} = decode(Bin, State),
  case Field of
    {not_found,Hash} ->
      {not_found,Hash};
    _ ->
      decode(field, Rest, Count - 1, Acc ++ [Field], _State)
  end.

get_type_set(#decoding_state{type_pool = Pool}) ->
  case dict:to_list(Pool) of
    [] -> [];
    Data -> [Value || {_Key,Value} <- Data]
  end.

build_foreign_view(ForeignType,FieldNames,State) ->
  logger:debug("[DECODE] build_foreign_view ForeignType ~p FieldNames ~p",[ForeignType,FieldNames]),
  ForeignView = FieldNames,
  #decoding_state{type_pool = OldPool} = State,
  Native = dict:size(OldPool),
  NewPool = dict:store(Native,
    #type_def{
      defineNo = Native,
%%            native_type = Native,
      foreign_type = ForeignType,
      fieldnames = ForeignView},
    OldPool),
  {
    #type_def{defineNo = Native,
%%            native_type = Native,
      foreign_type = ForeignType,
      fieldnames = ForeignView},
    State#decoding_state{type_pool = NewPool}
  }.

project_native_view(ForeignView,ForeignData,
    #type_def{native_type = _NativeType, foreign_type = _ForeignType, fieldnames = NativeView}) ->
  AsDict = dict:from_list(lists:zip(ForeignView,ForeignData)),
  NativeData = [dict:fetch(Key,AsDict) || Key <- NativeView],
  NativeData.

visit(TypeDef, State = #decoding_state{reference_pool = OldPool}) ->
  logger:debug("[DECODE] visit typedef ~p",[TypeDef]),
  Size = dict:size(OldPool),
  NewPool = dict:store(Size, TypeDef, OldPool),
  {Size,State#decoding_state{reference_pool = NewPool}}.

hash_lookup(Hash,#decoding_state{hash_pool = HashPool} = State) ->
  case dict:find(Hash,HashPool) of
    error->
      {not_found, Hash};
    {ok,TypeDef}->
      TypeDef
  end.

hash_store(TypeDef = #type_def{defineNo = Hash}, #decoding_state{hash_pool = HashPool} = State) ->
%%    init(false),
%%    ets:insert(hashes,{Hash,TypeDef}),
  logger:debug("[DECODE] hash store typedef ~p",[TypeDef]),
  NewPool = dict:store(Hash,TypeDef,HashPool),
  State#decoding_state{hash_pool = NewPool}.

init() -> init(true).

init(Delete) when is_boolean(Delete) ->
%%    case ets:info(hashes) of
%%        undefined ->
%%            ets:new(hashes,[private,named_table]);
%%        _ ->
%%            if
%%                Delete ->
%%                    ets:delete(hashes),
%%                    ets:new(hashes,[private,named_table]);
%%                true ->
%%                    ok
%%            end
%%    end,
  #decoding_state{}.

count_fields(#type_def{fieldnames = FieldNames}) -> length(FieldNames).

%% extend func
get_deftype(Ref,#decoding_state{type_pool = Pool})->
  dict:fetch(Ref,Pool).