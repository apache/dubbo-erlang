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
-module(dubbo_provider_consumer_reg_table).

-behaviour(gen_server).

%% API
-export([start_link/0, start_consumer/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([update_consumer_connections/2, update_node_conections/2, query_node_connections/1, get_interface_provider_node/1, get_host_connections/2, select_connection/1,
    update_connection_readonly/2, get_host_flag/1, get_host_flag/2, clean_invalid_provider/1, update_interface_info/1, get_interface_info/1]).

-include("dubbo.hrl").
-define(SERVER, ?MODULE).

-define(INTERFCE_LIST_TABLE, interface_list).

-define(INTERFACE_INFO_TABLE, dubbo_interface_info).

-define(PROVIDER_NODE_LIST_TABLE, provider_node_list).

-record(state, {}).

-ifdef(TEST).
-compile([export_all]).
-endif.


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
    init_ets_table(),
    {ok, #state{}}.
init_ets_table() ->
    try ets:new(?INTERFCE_LIST_TABLE, [bag, public, named_table, {keypos, 2}]) of
        ?INTERFCE_LIST_TABLE ->
            ok
    catch
        _Type:Reason ->
            logger:error("new ets table INTERFCE_LIST_TABLE error ~p", [Reason])
    end,
    try ets:new(?PROVIDER_NODE_LIST_TABLE, [bag, public, named_table, {keypos, 2}]) of
        ?PROVIDER_NODE_LIST_TABLE ->
            ok
    catch
        _Type1:Reason1 ->
            logger:error("new ets table  PROVIDER_NODE_LIST_TABLE error ~p", [Reason1])
    end,
    try ets:new(?INTERFACE_INFO_TABLE, [public, named_table, {keypos, 2}]) of
        ?INTERFACE_INFO_TABLE ->
            ok
    catch
        _Type2:Reason2 ->
            logger:error("new ets table  INTERFACE_INFO_TABLE error ~p", [Reason2])
    end,
    ok.
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

%%handle_call({add_consumer, Interface, ProviderNodeList}, _From, State) ->
%%
%%    OldProviderList = get_interface_provider_node(Interface),
%%    NewProviderList = add_consumer(ProviderNodeList, []),
%%    DeleteProverList = OldProviderList -- NewProviderList,
%%    clean_invalid_provider(DeleteProverList),
%%    {reply, ok, State};
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

start_consumer(Interface, ProviderNodeInfo) ->
    gen_server:call(?SERVER, {add_consumer, Interface, ProviderNodeInfo}).



get_host_connections(Host, Port) ->
    HostFlag = get_host_flag(Host, Port),
    List = ets:lookup(?PROVIDER_NODE_LIST_TABLE, HostFlag),
    List.



update_interface_info(InterfaceInfo) ->
    ets:insert(?INTERFACE_INFO_TABLE, InterfaceInfo).


get_interface_info(Interface) ->
    case ets:lookup(?INTERFACE_INFO_TABLE, Interface) of
        [] ->
            undefined;
        [Result] ->
            Result
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%add_consumer([], RegisterList) ->
%%    RegisterList;
%%add_consumer([ProviderNodeInfo | ProviderList], RegisterList) ->
%%    case dubbo_node_config_util:parse_provider_info(ProviderNodeInfo) of
%%        {ok, ProviderConfig} ->
%%            HostFlag = get_host_flag(ProviderConfig),
%%            case ets:lookup(?PROVIDER_NODE_LIST_TABLE, HostFlag) of
%%                [] ->
%%                    ConnectionList = start_provider_process(HostFlag, 30, ProviderConfig),
%%                    ok = update_connection_info(ProviderConfig#provider_config.interface, HostFlag, ConnectionList, true),
%%                    ok;
%%                List ->
%%                    List2 = lists:map(fun(#provider_node_list{connection_info = ConnectionItem}) ->
%%                        ConnectionItem
%%                                      end, List),
%%                    ok = update_connection_info(ProviderConfig#provider_config.interface, HostFlag, List2, false),
%%                    ok
%%            end,
%%            add_consumer(ProviderList, [HostFlag] ++ RegisterList);
%%        {error, R1} ->
%%            logger:error("parse provider info error reason ~p", [R1]),
%%            add_consumer(ProviderList, RegisterList)
%%    end.
%%
%%start_provider_process(HostFlag, Weight, ProviderConfig) ->
%%    ExecutesList = lists:seq(1, ProviderConfig#provider_config.executes),
%%    ConnectionList = lists:map(fun(Item) ->
%%        ConnectionFlag = <<HostFlag/binary, (integer_to_binary(Item))/binary>>,
%%        ConnectionFlagTerm = binary_to_atom(ConnectionFlag, utf8),
%%        AChild = {ConnectionFlagTerm, {dubbo_netty_client, start_link, [ConnectionFlagTerm, HostFlag, ProviderConfig, Item]}, permanent, 2000, worker, [dubbo_netty_client]},
%%        {ok, Pid} = dubbo_transport_pool_sup:add_children(AChild),
%%        logger:info("start provider ~p pid info ~p~n", [HostFlag, Pid]),
%%        #connection_info{connection_id = ConnectionFlagTerm, pid = Pid, weight = Weight, host_flag = HostFlag}
%%                               end, ExecutesList),
%%    ConnectionList.


update_node_conections(Interface, Connections) ->
    lists:map(
        fun(Item) ->
            HostFlag = Item#connection_info.host_flag,
            case ets:match_object(?PROVIDER_NODE_LIST_TABLE, #connection_info{host_flag = HostFlag, pid = Item#connection_info.pid, _ = "_"}) of
                [] ->
                    I2 = ets:insert(?PROVIDER_NODE_LIST_TABLE, Item),
                    logger:debug("update_node_conections insert one record ~p result:~p", [HostFlag, I2]);
                _ ->
                    logger:debug("update_node_conections hostflag ~p already exit ", [HostFlag]),
                    ok
            end
        end, Connections),
    ok.

query_node_connections(Hostflag) ->
    ets:lookup(?PROVIDER_NODE_LIST_TABLE, Hostflag).

update_consumer_connections(Interface, Connections) ->
    lists:map(
        fun(Item) ->
            I1 = ets:insert(?INTERFCE_LIST_TABLE, #interface_list{interface = Interface, pid = Item#connection_info.pid, connection_info = Item}),
            logger:debug("insert interface conection info ~p ~p ~p", [Interface, Item#connection_info.pid, I1]),
            ok
        end, Connections),
    ok.

get_host_flag(ProviderConfig) ->
    HostFlag = <<(ProviderConfig#provider_config.host)/binary, <<"_">>/binary, (integer_to_binary(ProviderConfig#provider_config.port))/binary>>,
    HostFlag.
get_host_flag(Host, Port) ->
    <<Host/binary, <<"_">>/binary, (integer_to_binary(Port))/binary>>.

update_connection_info(Interface, HostFlag, ConnectionList, IsUpdateProvideNode) ->
    lists:map(fun(Item) ->
        I1 = ets:insert(?INTERFCE_LIST_TABLE, #interface_list{interface = Interface, pid = Item#connection_info.pid, connection_info = Item}),
        logger:debug("insert interface conection info ~p ~p ~p", [Interface, Item#connection_info.pid, I1]),
        case IsUpdateProvideNode of
            true ->
                I2 = ets:insert(?PROVIDER_NODE_LIST_TABLE, Item),
                logger:debug("insert PROVIDER_NODE_LIST_TABLE ~p info:~p", [HostFlag, I2]);
            false ->
                ok
        end,
        ok
              end, ConnectionList),
    ok.

get_interface_provider_node(Interface) ->
    case ets:lookup(?INTERFCE_LIST_TABLE, Interface) of
        [] ->
            [];
        List ->
            ListRet = [Item#interface_list.connection_info#connection_info.host_flag || Item <- List],
            dubbo_lists_util:del_duplicate(ListRet)
    end.

select_connection(Interface) ->
    case ets:lookup(?INTERFCE_LIST_TABLE, Interface) of
        [] ->
            {error, none};
        List ->
            Ret = [Item#interface_list.connection_info || Item <- List],
            {ok, Ret}
    end.

-spec(update_connection_readonly(pid(), boolean()) -> ok).
update_connection_readonly(ConnectionPid, Readonly) ->
    Pattern = #interface_list{pid = ConnectionPid, _ = '_'},
    Objects = ets:match_object(?INTERFCE_LIST_TABLE, Pattern),
    lists:map(fun(#interface_list{interface = Interface, pid = Pid, connection_info = ConnectionInfo} = InterferConnection) ->
        logger:debug("[dubbo] update interface ~p ~p readonly", [Interface, Pid]),
        NewConnectionInfo = ConnectionInfo#connection_info{readonly = Readonly},
        NewObject = InterferConnection#interface_list{connection_info = NewConnectionInfo},
        ets:delete_object(?INTERFCE_LIST_TABLE, InterferConnection),
        ets:insert(?INTERFCE_LIST_TABLE, NewObject)
              end, Objects),
    {ok, length(Objects)}.

clean_invalid_provider([]) ->
    ok;
clean_invalid_provider([HostFlag | DeleteProverList]) ->
    case ets:lookup(?PROVIDER_NODE_LIST_TABLE, HostFlag) of
        [] ->
            ok;
        ProviderNodeConnections ->
            ProviderNodeConnections1 = dubbo_lists_util:del_duplicate(ProviderNodeConnections),
            clean_connection_info(ProviderNodeConnections1)
    end,
    clean_invalid_provider(DeleteProverList).

clean_connection_info(ProviderNodeConnections) ->
    lists:map(fun(Item) ->
        Pid = Item#connection_info.pid,
        Pattern = #interface_list{pid = Pid, _ = '_'},
        ets:delete_object(?INTERFCE_LIST_TABLE, Pattern),
        dubbo_transport_pool_sup:stop_children(Pid)
              end, ProviderNodeConnections),
    ok.