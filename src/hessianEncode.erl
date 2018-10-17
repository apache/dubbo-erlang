-module(hessianEncode).

-include("hessian.hrl").

-export([encode/2, encode/3, encode/4, encode/5]).
-export([get_value/3, get_value/4]).
-export([wrap_class/1, encode_for_decode/2,encode_object/3]).

%---------------------------------------------------------------------------
% Encoding
%---------------------------------------------------------------------------
encode_for_decode(Value, [{set,SetInfo},ClassList]) when is_tuple(Value) -> encode(struct, Value, [{set,SetInfo},wrap_class(ClassList)]);
encode_for_decode(Value, State) when is_tuple(Value) -> encode(struct, Value, wrap_class(State)).
%---------------------------------------------------------------------------
% encode/2	<--	** Entry **
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
            lager:debug("[encode] object ~p",[Input]),
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
				%% todo 还未验证
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
			lager:debug("[encode] encode object TypeNo ~p",[TypeNo]),
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
    lager:debug("[encode] encode_accumulate value ~p",[Value]),
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

get_class(Where, ClassList) ->
	SortClassList = lists:reverse(ClassList),
	case Where#object.typeRef of
		-1 ->
			if
			Where#object.class =:= auto -> hd(ClassList);
			true ->
				Index = type_index(Where#object.class, ClassList),
				lists:nth(Index+1, SortClassList)
			end;
		TypeRef ->
			lists:nth(TypeRef+1, SortClassList)
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

wrap_class(ClassList) ->
	Fun = fun(OldClass, AccIn) -> 
		[#class{typeNo=OldClass#type_def.native_type, name=OldClass#type_def.foreign_type, fields=OldClass#type_def.fieldnames}|AccIn]
	end,
	lists:foldl(Fun, [], ClassList).

get_value(Where, Ref, Field) ->
	get_value(Where, Ref, Field, [], []).
get_value(Where, Ref, Field, ClassList) ->
	get_value(Where, Ref, Field, wrap_class(ClassList), []).
get_value(Where, Ref, Field, ClassList, ObjVlues) when is_record(Where, list) ->
	get_value(Where#list.values, Ref, Field, ClassList, ObjVlues);
get_value(Where, Ref, Field, ClassList, _ObjVlues) when is_record(Where, map) ->
	List = dict:to_list(Where#map.dict),
	get_value(List, Ref, Field, ClassList, Where#map.refNo);
get_value(Where, _Ref, _Field, ClassList, _ObjVlues) when is_record(Where, type_def) ->
	{class, [#class{typeNo=Where#type_def.native_type, name=Where#type_def.foreign_type, fields=Where#type_def.fieldnames}|ClassList]};
get_value(Where, _Ref, _Field, ClassList, _ObjVlues) when is_record(Where, class) ->
	{class, [Where|ClassList]};
get_value(Where, Ref, Field, ClassList, _ObjVlues) when is_record(Where, object) ->
	Class = get_class(Where, ClassList),
	if
		Where#object.refNo =:= Ref -> get_value(Class#class.fields, Ref, Field, ClassList, Where#object.values);
		Class =:= Ref -> get_value(Class#class.fields, Ref, Field, ClassList, Where#object.values);
		true ->
			case equal(Class#class.name, Ref) of
				true -> get_value(Class#class.fields, Ref, Field, ClassList, Where#object.values);
				false -> get_value(Where#object.values, Ref, Field, ClassList, skip)
			end
	end;
get_value(Where, _RefNo, _Field, _ClassList, skip) when is_binary(Where) ->
	next;
get_value(Where, _RefNo, Field, _ClassList, Value) when is_binary(Where) ->
	case equal(Where, Field) of
		true -> {return, Value};
		false -> next
	end;
get_value(Where, _RefNo, _Field, _ClassList, _Value) when is_atom(Where) ->
	next;
get_value(Where, _RefNo, _Field, _ClassList, _Value) when is_integer(Where) ->
	next;
get_value({ref, _No}, _RefNo, _Field, _ClassList, _ObjVlues) ->
	next;
get_value({K, V}, RefNo, Field, ClassList, MapRefNo) ->
	case is_binary(K) of
		true ->
			if
				RefNo =/= MapRefNo -> R = next;
				true -> R = get_value(K, RefNo, Field, ClassList, V)
			end;
		false -> R = get_value(K, RefNo, Field, ClassList, [])
	end,
	case R of
		next -> get_value(V, RefNo, Field, ClassList, []);
		_ -> R
	end;
get_value([], _RefNo, _Field, _ClassList, _ObjVlues) ->
	not_found;
get_value([CurrentField|Fields], RefNo, Field, ClassList, [Value|Values]) ->
	R = get_value(CurrentField, RefNo, Field, ClassList, Value),
	case R of
		{return, _} -> R;
		{class, NewClassList} -> get_value(Fields, RefNo, Field, NewClassList, Values);
		_ -> get_value(Fields, RefNo, Field, ClassList, Values)
	end;
get_value([Item|T], RefNo, Field, ClassList, Option)->
	R = get_value(Item, RefNo, Field, ClassList, Option),
	case R of
		{return, _} -> R;
		{class, NewClassList} -> get_value(T, RefNo, Field, NewClassList, Option);
		_ -> get_value(T, RefNo, Field, ClassList, Option)
	end.

encode_object(object, Object, State) when is_tuple(Object) ->
	[NativeType|Values] = tuple_to_list(Object),
	{TypeEncoding, EncodedRef, NewState} =
		case type_encoding:visit(NativeType,State) of
			{ref, Ref} ->
				encode_object(type_information, {ref, Ref}, State);
			{hash, Hash, Ref, State0} ->
				encode(type_information, {hash, Hash, Ref}, State0)
		end,
	{AccOut, _NewState} = lists:foldl(fun encode/2,{<<>>, NewState},Values),
	{<<TypeEncoding/binary,$o,EncodedRef/binary,AccOut/binary>>, _NewState};
encode_object(type_information, {ref, Ref}, State) ->
	{<<>>, encode(int, Ref, State), State};
encode_object(type_information, {hash, Hash, Ref}, State) ->
	EncodedRef = encode(int, Ref, State),
	{<<$O,Hash:32/unsigned>>, EncodedRef, State}.

typedef_to_class(Typedef,DefineNo)->
	#class{typeNo=DefineNo, name=Typedef#type_def.foreign_type, fields=Typedef#type_def.fieldnames}.