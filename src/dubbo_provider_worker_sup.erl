%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2018 8:04 PM
%%%-------------------------------------------------------------------
-module(dubbo_provider_worker_sup).
-author("dlive").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'AName', {'AModule', start_link, []},
        Restart, Shutdown, Type, ['AModule']},

    PoolArgs = [{name, {local, provider_worker}},
        {worker_module, dubbo_provider_worker},
        {size, 5},
        {max_overflow, 100}
    ],
    WorkerPool = poolboy:child_spec(provider_worker, PoolArgs, []),
    {ok, {SupFlags, [WorkerPool]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%start_child(InterfaceList)->
%%    ChildSpec = {dubbo_provider_server,{dubbo_provider_server,start_link,[InterfaceList]},permanent,2000,worker},
%%    supervisor:start_child(?SERVER,ChildSpec),
%%    ok.