


-module(time_util).

-include_lib("kernel/include/file.hrl").


-export([
	get_cur_time/0,get_cur_time/1,
	format_time_to_str/1,
	timestamp/0,timestamp_ms/0,
	timestamp_to_datetime/1,
	timestamp_to_local_datetime/1,
	get_cur_date/0,
	datetime_to_timestamp/1,
	datetime_string_to_timestamp/1,get_curdate_timestamp/0]).


get_cur_time()->
	{{Year,Month,Day},{Hour,Min,Second}}=calendar:now_to_local_time(os:timestamp()),
	io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour,Min,Second]).

get_cur_date()->
	{{Year,Month,Day},{Hour,Min,Second}}=calendar:now_to_local_time(os:timestamp()),
	io_lib:format("~4..0w-~2..0w-~2..0w",[Year, Month, Day]).

get_cur_time({{Year,Month,Day},{Hour,Min,Second}})->
	io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour,Min,Second]).

format_time_to_str({{Year,Month,Day},{Hour,Min,Second}})->
	io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour,Min,Second]).

timestamp() ->  
    {M, S, _} = os:timestamp(),  
    M * 1000000 + S.
timestamp_ms()->
    {M,S,W} = os:timestamp(),
    M*1000000000+S*1000+(W div 1000).

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +  
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).
timestamp_to_local_datetime(Timestamp) ->
    Date=calendar:gregorian_seconds_to_datetime(Timestamp +  
      calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Date).

%% @doc 时间转时间戳
datetime_to_timestamp(Date)->
	[{D,T}]=calendar:local_time_to_universal_time_dst(Date),
	S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1).
    % {Seconds div 1000000, Seconds rem 1000000, MS}.

%% @doc 时间字符串转 时间戳
datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
    {'EXIT', _Err} ->
        undefined;
    TimeStamp ->
        TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
    true ->
        Date1;
    _ ->
        false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
    0 ->
        parse_time_with_timezone(Time);
    _ ->
        [T | _] = string:tokens(Time, "Z"),
        {TT, MS} = parse_time1(T),
        {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
    0 ->
        case string:str(Time, "-") of
        0 ->
            false;
        _ ->
            parse_time_with_timezone(Time, "-")
        end;
    _ ->
        parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
    "-" ->
        {TT, MS, -TZH, -TZM};
    "+" ->
        {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
         [] ->
         0;
         [Val] ->
         list_to_integer(string:left(Val, 6, $0))
     end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
      V = list_to_integer(L),
      if
          (V >= 0) and (V =< N) ->
          {V, B};
          true ->
          {false, false}
      end
      end, true, List).
%% @doc 获取当天零点时间戳
get_curdate_timestamp()->
    {M, S, T} = os:timestamp(),
    TimestampTmp = M * 1000000 + S,
    {{Year,Month,Day},{Hour,Min,Second}}=calendar:now_to_local_time({M, S, T}),
    TimestampTmp - Second -(Hour*60+Min)*60.