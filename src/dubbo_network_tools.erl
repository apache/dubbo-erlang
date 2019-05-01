%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2018 4:07 PM
%%%-------------------------------------------------------------------
-module(dubbo_network_tools).
-author("dlive").

%% API
-export([local_ip_v4/0,local_ipv4/0,local_ipv4_binary/0]).


local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

local_ipv4_binary()->
    {I1,I2,I3,I4}=local_ip_v4(),
    list_to_binary(io_lib:format("~p.~p.~p.~p",[I1,I2,I3,I4])).
local_ipv4()->
    {I1,I2,I3,I4}=local_ip_v4(),
    lists:flatten(io_lib:format("~p.~p.~p.~p",[I1,I2,I3,I4])).
