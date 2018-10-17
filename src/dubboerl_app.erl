%%%-------------------------------------------------------------------
%% @doc dubboerl public API
%% @end
%%%-------------------------------------------------------------------

-module(dubboerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,env_init/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("[START] server start~n"),
    env_init(),
    dubboerl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
env_init()->
    dubbo_traffic_control:init(),
    type_register:init(),
    register_type_list().
%%    type_decoding:init().


register_type_list()->
    List = java_type_defined:get_list(),
    lists:map(
        fun({NativeType,ForeignType,Fields}) ->
            de_type_transfer:pre_process_typedef(NativeType,ForeignType,Fields)
        end,List),
    ok.