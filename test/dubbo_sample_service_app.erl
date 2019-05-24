%%%-------------------------------------------------------------------
%% @doc dubbo_sample_service public API
%% @end
%%%-------------------------------------------------------------------

-module(dubbo_sample_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("dubbo_sample_service.hrl").
-export([register_type_list/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    register_type_list(),
    dubbo_sample_service_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


register_type_list()->
    List = dubbo_sample_service_type_list:get_list(),
    lists:map(
        fun({NativeType,ForeignType,Fields}) ->
        dubbo_type_transfer:pre_process_typedef(NativeType,ForeignType,Fields)
    end,List),
    ok.