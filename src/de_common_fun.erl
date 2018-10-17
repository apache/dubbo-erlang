%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2017 3:01 PM
%%%-------------------------------------------------------------------
-module(de_common_fun).
-author("dlive").

%% API
-export([local_ip_v4/0,local_ip_v4_str/0]).

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

local_ip_v4_str()->
    {V1,V2,V3,V4} =local_ip_v4(),
    list_to_binary(io_lib:format("~p.~p.~p.~p",[V1,V2,V3,V4])).
