%%%-------------------------------------------------------------------
%% @doc dubboerl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dubboerl_sup).

-behaviour(supervisor).

-include("common.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ZK = {dubbo_zookeeper,{dubbo_zookeeper, start_link, []},transient,5000,worker,[dubbo_zookeeper]},
%%    NettySer = {dubbo_netty_client,{dubbo_netty_client, start_link, []},transient,5000,worker,[dubbo_netty_client]},
    Id_count = {dubbo_id_generator,{dubbo_id_generator, start_link, []},transient,5000,worker,[dubbo_id_generator]},
    ProviderPoolSup = {dubbo_provider_worker_sup,{dubbo_provider_worker_sup, start_link, []},transient,5000,supervisor,[dubbo_provider_worker_sup]},
    ConsumerPoolSup = {dubbo_consumer_pool_sup,{dubbo_consumer_pool_sup, start_link, []},transient,5000,supervisor,[dubbo_consumer_pool_sup]},
    ConsumerPool = {dubbo_consumer_pool,{dubbo_consumer_pool, start_link, []},transient,5000,worker,[dubbo_consumer_pool]},
%%    Reloader = {push_reloader,{push_reloader, start_link, []},transient,5000,worker,[push_reloader]},
%%    List = case ?RELOADER of
%%               true ->
%%                   io:format("[START] will start push_reloader service2222~n"),
%%                   [Reloader];
%%               _->[]
%%           end,
    ListNew1=
        case application:get_env(dubboerl,registry,false) of
            true ->
                [ZK];
            false->
                []
        end,
    ListNew = ListNew1 ++ [Id_count,ConsumerPool,ConsumerPoolSup,ProviderPoolSup],
    {ok, { {one_for_one, 60, 10}, ListNew} }.

%%====================================================================
%% Internal functions
%%====================================================================
