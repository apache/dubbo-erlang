%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Dec 2017 9:58 AM
%%%-------------------------------------------------------------------
-module(dubbo_consumer_pool_sup).
-author("dlive").

-behaviour(supervisor).

%% API
-export([start_link/0,add_children/1,stop_children/1]).

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

%%    AChild = {'AName', {'AModule', start_link, []}, Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, []}}.


add_children(ChildSpec)->
    supervisor:start_child(?SERVER,ChildSpec).
stop_children(ChildID)->
    supervisor:terminate_child(?SERVER,ChildID).
%%%===================================================================
%%% Internal functions
%%%===================================================================
