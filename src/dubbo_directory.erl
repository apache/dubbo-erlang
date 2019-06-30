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
-module(dubbo_directory).

-behaviour(gen_server).
-include("dubboerl.hrl").
-include("dubbo.hrl").

-export([subscribe/2, notify/2]).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

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
    {ok, #state{}}.

subscribe(RegistryName, SubcribeUrl) ->
    RegistryName:subscribe(SubcribeUrl, fun dubbo_directory:notify/2),
    ok.

notify(Interface, []) ->
    logger:info("[DUBBO] directory get notify, interface provider list is empty"),
    ok;
notify(Interface, UrlList) ->
    refresh_invoker(UrlList),
    ok.


refresh_invoker(UrlList) ->
    case pick_interface(UrlList) of
        {error, Reason} ->
            fail;
        {<<"empty">>, Interface,_} ->
            OldProviderHosts = dubbo_provider_consumer_reg_table:get_interface_provider_node(Interface),
            dubbo_provider_consumer_reg_table:clean_invalid_provider(OldProviderHosts),
            todo_destroy;
        {Schame, Interface, LoadBalance} ->
            ProtocolModule = binary_to_existing_atom(<<<<"dubbo_protocol_">>/binary, Schame/binary>>, latin1),

            logger:info("[DUBBO] refresh invoker for interface ~p loadbalance ~p protocol ~p", [Interface, LoadBalance, ProtocolModule]),
            OldProviderHosts = dubbo_provider_consumer_reg_table:get_interface_provider_node(Interface),
            NewInvokers = refresh_invoker(UrlList, []),
            NewProviderHosts = [Item#dubbo_invoker.host_flag || Item <- NewInvokers],
            DeleteProverList = OldProviderHosts -- NewProviderHosts,
            dubbo_provider_consumer_reg_table:clean_invalid_provider(DeleteProverList),

            lists:map(
                fun(NewHosts) ->
                    NewHostConnections = dubbo_provider_consumer_reg_table:query_node_connections(NewHosts),
                    dubbo_provider_consumer_reg_table:update_consumer_connections(Interface, NewHostConnections)
                end, NewProviderHosts),


%%            dubbo_provider_consumer_reg_table:update_connection_info(#interface_info{interface = Interface,loadbalance = LoadBalance})
            dubbo_provider_consumer_reg_table:update_interface_info(#interface_info{interface = Interface, loadbalance = LoadBalance, protocol = ProtocolModule})
    end.
%%    OldProviderHosts =

refresh_invoker([], Acc) ->
    Acc;
refresh_invoker([Url | Rest], Acc) ->
    logger:info("refresh invoker ~s", [Url]),
    case dubbo_extension:run_fold(protocol, refer, [Url], undefined) of
        undefined ->
            refresh_invoker(Rest, Acc);
        {ok, Invoker} ->
            refresh_invoker(Rest, [Invoker | Acc]);
        {stop, _} ->
            refresh_invoker(Rest, Acc)
    end.

pick_interface([Url | _]) ->
    case dubbo_common_fun:parse_url(Url) of
        {ok, UrlInfo} ->
            logger:debug("pick interface info from ~p", [Url]),
            Interface = maps:get(<<"interface">>, UrlInfo#dubbo_url.parameters),
            LoadBalanceName = maps:get(<<"loadbalance">>, UrlInfo#dubbo_url.parameters, <<"random">>),
            LoadBalance = binary_to_existing_atom(<<<<"dubbo_loadbalance_">>/binary, LoadBalanceName/binary>>, latin1),
            {UrlInfo#dubbo_url.scheme, Interface, LoadBalance};
        {error, Reason} ->
            {error, Reason}
    end.


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
handle_call({subscribe, RegistryName, SubcribeUrl}, _From, State) ->
    NotifyFun = fun dubbo_directory:notify/1,
    apply(RegistryName, subscribe, [SubcribeUrl, NotifyFun]),
    {reply, ok, State};
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

