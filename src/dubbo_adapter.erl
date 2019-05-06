%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 00:27
%%%-------------------------------------------------------------------
-module(dubbo_adapter).
-author("dlive").

-include("dubbo.hrl").
%% API
-export([reference/1]).

-spec reference(Data::#dubbo_rpc_invocation{}) -> #dubbo_request{}.
reference(Data)->
  #dubbo_request{
    is_event = false,
    is_twoway = true,
    mid = dubbo_id_generator:gen_id(),
    data = Data,
    mversion= <<"0.0.0">>,
    serialize_type = serialize_value(application:get_env(dubboerl,protocol,hessian))
  }.

serialize_value(json)->
  ?SERIALIZATION_FASTJSON;
serialize_value(_)->
  ?SERIALIZATION_HESSIAN.
