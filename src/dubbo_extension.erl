%%------------------------------------------------------------------------------
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed with
%% this work for additional information regarding copyright ownership.
%% The ASF licenses this file to You under the Apache License, Version 2.0
%% (the "License"); you may not use this file except in compliance with
%% the License.  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------
-module(dubbo_extension).
-behaviour(gen_server).

%% API
-export([run/3, run_fold/4, run_fold/5, register/3, unregister/3, invoke/5, invoke_foldr/4]).


-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(TAB, ?MODULE).

-record(state, {}).


-spec register(HookName :: atom(), Module :: atom(), Priority :: integer()) -> ok | {error, term()}.
register(HookName, Module, Priority) ->
    gen_server:call(?MODULE, {register, HookName, {Priority, Module}}).

-spec unregister(HookName :: atom(), Module :: atom(), Priority :: integer()) -> ok.
unregister(HookName, Module, Priority) ->
    gen_server:call(?MODULE, {unregister, HookName, {Priority, Module}}).

-spec run(HookName :: atom(), Fun :: atom(), Args :: list()) -> ok.
run(HookName, Fun, Args) ->
    case find_hooks(HookName) of
        no_hook -> ok;
        Hooks ->
            run1(Hooks, HookName, Fun, Args)
    end.

run1([], _HookName, _Fun, _Args) ->
    ok;
run1([M | Rest], HookName, Fun, Args) ->
    Ret = (catch apply(M, Fun, Args)),
    case Ret of
        {'EXIT', Reason} ->
            io:format(user, "~p~n error running extension: ~p~n", [HookName, Reason]),
            logger:error("~p~n error running extension: ~p~n", [HookName, Reason]),
            run1(Rest, HookName, Fun, Args);
        stop ->
            ok;
        _ ->
            run1(Rest, HookName, Fun, Args)
    end.

-spec run_fold(HookName :: atom(), Fun :: atom(), Args :: list(), Acc :: any()) -> Acc2 :: any().
run_fold(HookName, Fun, Args, Acc) ->
    case find_hooks(HookName) of
        no_hook -> Acc;
        Hooks -> run_fold1(Hooks, HookName, Fun, Args, Acc)
    end.

run_fold(HookName, Fun, Args, Acc, AppendExtension) ->
    case find_hooks(HookName) of
        no_hook -> Acc;
        Hooks ->
            run_fold1(Hooks ++ AppendExtension, HookName, Fun, Args, Acc)
    end.

run_fold1([], _HookName, _Fun, _Args, Acc) ->
    Acc;
run_fold1([M | Rest], HookName, Fun, Args0, Acc) ->
    Args = Args0 ++ [Acc],
    Ret = (catch apply(M, Fun, Args)),
    case Ret of
        {'EXIT', Reason} ->
            logger:error("~p~n error running hook: ~p~n", [HookName, Reason]),
            run_fold1(Rest, HookName, Fun, Args0, Acc);
        stop ->
            Acc;
        {stop, NewAcc} ->
            NewAcc;
        _ ->
            run_fold1(Rest, HookName, Fun, Args0, Ret)
    end.

invoke_foldr(HookName, Fun, Args, Acc) ->
    case find_hooks(HookName) of
        no_hook -> Acc;
        Hooks ->
            do_invoke(lists:reverse(Hooks), HookName, Fun, Args, Acc)
    end.

invoke(HookName, Fun, Args, Acc, AppendExtension) ->
    case find_hooks(HookName) of
        no_hook -> Acc;
        Hooks ->
            do_invoke(Hooks ++ AppendExtension, HookName, Fun, Args, Acc)
    end.

do_invoke([], _HookName, _Fun, _Args, Acc) ->
    Acc;
do_invoke([M | Rest], HookName, Fun, Args0, Acc) ->
    Args = Args0 ++ [Acc],
    Ret = (catch apply(M, Fun, Args)),
    case Ret of
        {'EXIT', Reason} ->
            logger:error("~p~n error running hook: ~p~n", [HookName, Reason]),
            do_invoke(Rest, HookName, Fun, Args0, Acc);
        stop ->
            Acc;
        {stop, NewAcc} ->
            NewAcc;
        {ok, Args2, NewAcc2} ->
            do_invoke(Rest, HookName, Fun, [Args2], NewAcc2)
    end.


%% @doc retrieve the lists of registered functions for an hook.
-spec find(HookName :: atom()) -> {ok, [{atom(), atom()}]} | error.
find(HookName) ->
    case find_hooks(HookName) of
        no_hook -> error;
        Hooks -> {ok, Hooks}
    end.

%% @hidden
start_link() ->
    _ = init_tabs(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init_tabs() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, public, named_table,
                {read_concurrency, true},
                {write_concurrency, true}]);
        _ ->
            true
    end.

%% @hidden
init([]) ->
    {ok, #state{}}.

%% @hidden
handle_call({register, HookName, {Priority, Module}}, _From, State) ->
    do_register(HookName, {Priority, Module}),
    {reply, ok, State};
handle_call({unregister, HookName, {Priority, Module}}, _From, State) ->
    do_unregister(HookName, {Priority, Module}),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
terminate(_Reason, _Srv) ->
    ok.

do_register(HookName, {_Priority, ModuleName} = Hook) ->
    check_module(ModuleName),
    update_hooks(HookName, [Hook]).


do_unregister(HookName, Hook) ->
    remove_hooks(HookName, [Hook]),
    ok.

update_hooks(HookName, HookFuns) ->
    case ets:lookup(?TAB, HookName) of
        [] ->
            true = ets:insert(?TAB, {HookName, HookFuns});
        [{_, Funs}] ->
            Funs2 = lists:keysort(1, Funs ++ HookFuns),
            true = ets:insert(?TAB, {HookName, Funs2})
    end.

remove_hooks(HookName, HookFuns) ->
    case ets:lookup(?TAB, HookName) of
        [] ->
            ok;
        [{_, Funs}] ->
            Funs2 = Funs -- HookFuns,
            case Funs2 of
                [] ->
                    ets:delete(?TAB, HookName);
                _ ->
                    ets:insert(?TAB, {HookName, Funs2})
            end
    end.

check_module(ModuleName) ->
    _ = code:ensure_loaded(ModuleName),
    ok.

find_hooks(HookName) ->
    case ets:lookup(?TAB, HookName) of
        [] ->
            no_hook;
        [{_, Modules}] ->
            Modules1 = [Module || {_, Module} <- Modules],
            Modules1
    end.
