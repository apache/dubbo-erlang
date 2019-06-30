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
-module(dubbo_registry_zookeeper).
-behaviour(gen_server).
-behaviour(dubbo_registry).

-include("dubbo.hrl").
-include("dubboerl.hrl").
%% API
-export([start_link/0, register_provider/1, provider_watcher/1]).

-export([start/1, register/1, unregister/1, subscribe/2]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {zk_pid, provider_notify_fun}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, Pid} = connection(),
    {ok, #state{zk_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({do_register, Url}, _From, State) ->
    do_register(State#state.zk_pid, Url),
    {reply, ok, State};
handle_call({do_unregister, Url}, _From, State) ->
    do_unregister(State#state.zk_pid, Url),
    {reply, ok, State};
handle_call({subscribe_provider, InterfaceName, NotifyFun}, _From, #state{zk_pid = ZkPid} = State) ->
    logger:debug("subscribe provider ~p notify fun ~p",[InterfaceName,NotifyFun]),
    NewState = State#state{provider_notify_fun = NotifyFun},
    List = get_provider_list(InterfaceName, ZkPid),
    notify_provider_change(NotifyFun, InterfaceName, List),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({provider_node_change, Interface, Path}, #state{zk_pid = Pid, provider_notify_fun = NotifyFun} = State) ->
    ProviderList = get_provider_and_start(Pid, Interface, Path),
    notify_provider_change(NotifyFun, Interface, ProviderList),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%----------------------------------------------
%% dubbo_registry
%%----------------------------------------------
start(Url) ->
    ok.
%%register(Url) ->
%%    {ok, UrlInfo} = dubbo_common_fun:parse_url(Url),
%%    InterfaceName = maps:get(<<"interface">>, UrlInfo#dubbo_url.parameters),
%%    register(UrlInfo#dubbo_url.scheme, InterfaceName, Url),
%%    ok.

%%register(<<"consumer">>, InterfaceName, Url) ->
%%    gen_server:call(?SERVER, {add_consumer, InterfaceName, Url}),
%%    ok.

register(Url) ->
    gen_server:call(?SERVER, {do_register, Url}, 10000),
    ok.

unregister(Url) ->
    gen_server:call(?SERVER, {do_unregister, Url}, 10000),
    ok.

do_register(Pid, Url) ->
    case dubbo_common_fun:parse_url(Url) of
        {ok, UrlInfo} ->
            CreateNodeList = [{get_register_node(Item, UrlInfo), p} || Item <- [root, service, category]],
            UrlNode = {list_to_binary(edoc_lib:escape_uri(binary_to_list(Url))), get_dynamic(UrlInfo)},
            CreateNodeList2 = CreateNodeList ++ [UrlNode],
            RetFullNode = check_and_create_path(Pid, <<"">>, CreateNodeList2),
            {ok, RetFullNode};
        Reason ->
            logger:error("zk parse url fail reason ~p", [Reason]),
            {error, Reason}
    end.
do_unregister(Pid, Url) ->
    case dubbo_common_fun:parse_url(Url) of
        {ok, UrlInfo} ->
            CreateNodeList = [get_register_node(Item, UrlInfo) || Item <- [root, service, category]],
            UrlNode = list_to_binary(edoc_lib:escape_uri(binary_to_list(Url))),
            CreateNodeList2 = CreateNodeList ++ [UrlNode],
            Path = dubbo_common_fun:binary_list_join(CreateNodeList2, <<"/">>),
            FullPath = <<<<"/">>/binary, Path/binary>>,
            del_path(Pid, FullPath);
        Reason ->
            logger:error("zk parse url fail reason ~p", [Reason]),
            {error, Reason}
    end.

get_dynamic(UrlInfo) ->
    case maps:get(<<"dynamic">>, UrlInfo#dubbo_url.parameters, <<"true">>) of
        <<"true">> ->
            e;
        _ ->
            p
    end.

get_register_node(root, _UrlInfo) ->
    <<"dubbo">>;
get_register_node(service, UrlInfo) ->
    maps:get(<<"interface">>, UrlInfo#dubbo_url.parameters);
get_register_node(category, UrlInfo) ->
    maps:get(<<"category">>, UrlInfo#dubbo_url.parameters, <<"providers">>).


subscribe(SubcribeUrl, NotifyFun) ->
    {ok, UrlInfo} = dubbo_common_fun:parse_url(SubcribeUrl),
    InterfaceName = maps:get(<<"interface">>, UrlInfo#dubbo_url.parameters),
    try gen_server:call(?SERVER, {subscribe_provider, InterfaceName, NotifyFun}, 5000) of
        ok ->
            ok
    catch
        _Error:Reason ->
            %%todo improve error type
            {error, Reason}
    end.


register_provider(Provider) ->
    gen_server:call(?SERVER, {add_provider, Provider}),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connection() ->
    {ok, List} = application:get_env(dubboerl, zookeeper_list),
    {ok, Pid} = erlzk:connect(List, 30000, [
        {chroot, "/"},
        {monitor, self()}]),
    {ok, Pid}.

add_consumer(InterfaceName, ConsumerUrl, State) ->
    Pid = State#state.zk_pid,
    ConsumerNode2 = list_to_binary(edoc_lib:escape_uri(binary_to_list(ConsumerUrl))),
    check_and_create_path(Pid, <<"">>, [{<<"dubbo">>, p}, {InterfaceName, p}, {<<"consumers">>, p}, {ConsumerNode2, e}]),
    %% todo
%%    get_provider_list(Consumer, State),
    ok.
register_provider_path(Provider, State) ->
    #state{zk_pid = Pid} = State,
    ProviderNode = dubbo_node_config_util:gen_provider_info(Provider),
    check_and_create_path(Pid, <<"">>, [{<<"dubbo">>, p}, {Provider#provider_config.interface, p}, {<<"providers">>, p}, {ProviderNode, e}]),
    ok.


get_provider_list(InterfaceName, ZkPid) ->
    InterfacePath = <<<<"/dubbo/">>/binary, InterfaceName/binary, <<"/providers">>/binary>>,
    ChildList = get_provider_and_start(ZkPid, InterfaceName, InterfacePath),
    ChildList.
get_provider_and_start(Pid, Interface, Path) ->
    case erlzk:get_children(Pid, Path, spawn(dubbo_registry_zookeeper, provider_watcher, [Interface])) of
        {ok, ChildList} ->
            logger:debug("get provider list ~p", [ChildList]),
            ChildList;
        {error, no_node} ->
            logger:warning("interface ~p provide zk node unexist", [Interface]),
            check_and_create_path(Pid, <<"">>, [{<<"dubbo">>, p}, {Interface, p}, {<<"providers">>, p}]),
            get_provider_and_start(Pid, Interface, Path);
        {error, R1} ->
            logger:debug("[add_consumer] get_provider_list error ~p", [R1]),
            []
    end.

provider_watcher(Interface) ->
    receive
        {node_children_changed, Path} ->
            gen_server:cast(?SERVER, {provider_node_change, Interface, Path}),
            logger:debug("provider_watcher get event ~p ~p", [node_children_changed, Path]);
        {Event, Path} ->
            logger:debug("provider_watcher get event ~p ~p", [Event, Path])
    end,
    ok.

notify_provider_change(Fun, Interface, []) ->
    UrlInfo = #dubbo_url{
        scheme = <<"empty">>,
        host = <<"127.0.0.1">>,
        path = Interface,
        port = 80,
        parameters = #{
            <<"interface">> => Interface
        }
    },
    UrlInfoBin = dubbo_common_fun:url_to_binary(UrlInfo),
    logger:debug("notify provider change fun ~p", [Fun]),
    Fun(Interface, [UrlInfoBin]),
    ok;
notify_provider_change(Fun, Interface, List) ->
    List2 = [http_uri:decode(Item) || Item <- List],
    logger:debug("notify provider change fun ~p", [Fun]),
    Fun(Interface, List2),
    ok.

del_path(Pid, Path) ->
    case erlzk:delete(Pid, Path) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("zookeeper registry del path error ~p path ~p", [Reason, Path]),
            {error, Reason}
    end.

create_path(Pid, Path, CreateType) ->
    case erlzk:create(Pid, Path, CreateType) of
        {ok, ActualPath} ->
            logger:debug("create zk path  success ~p", [ActualPath]),
            ok;
        {error, R1} ->
            logger:debug("create zk path error ~p ~p", [Path, R1])
    end,
    ok.
check_and_create_path(_Pid, RootPath, []) ->
    RootPath;
check_and_create_path(Pid, RootPath, [{Item, CreateType} | Rst]) ->
    CheckPath = <<RootPath/binary, <<"/">>/binary, Item/binary>>,

    case erlzk:exists(Pid, CheckPath) of
        {ok, Stat} ->
            check_and_create_path(Pid, CheckPath, Rst);
        {error, no_node} ->
            logger:debug("check_and_create_path unexist no_node ~p", [CheckPath]),
            create_path(Pid, CheckPath, CreateType),
            check_and_create_path(Pid, CheckPath, Rst);
        {error, R1} ->
            logger:debug("check_and_create_path unexist ~p", [R1]),
            check_and_create_path(Pid, CheckPath, Rst)
    end.