-module(hessianDecode2).

-include("hessian.hrl").

-export([decode/2]).
-export([init/0]).
-export([get_deftype/2]).
-record(decoding_state,{type_pool = dict:new(), reference_pool = dict:new(),hash_pool = dict:new()}).

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
    lager:debug("decode data find ref ~p",[Ref]),
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
    {error, hessianEncode:encode(fault, <<"ProtocolException">>, unexpected_byte_sequence, Unexpected, State) }.
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
    lager:debug("[DECODE] build_foreign_view ForeignType ~p FieldNames ~p",[ForeignType,FieldNames]),
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
    lager:debug("[DECODE] visit typedef ~p",[TypeDef]),
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
%%    init(false),
%%    case ets:lookup(hashes, Hash) of
%%        [] ->
%%            {not_found, Hash};
%%        [{Hash,TypeDef}] ->
%%            TypeDef
%%    end.

hash_store(TypeDef = #type_def{defineNo = Hash}, #decoding_state{hash_pool = HashPool} = State) ->
%%    init(false),
%%    ets:insert(hashes,{Hash,TypeDef}),
    lager:debug("[DECODE] hash store typedef ~p",[TypeDef]),
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