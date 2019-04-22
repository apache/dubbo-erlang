%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十月 2016 上午9:56
%%%-------------------------------------------------------------------
-module(dubbo_zookeeper).
-author("dlive").

-behaviour(gen_server).

-include("dubbo.hrl").
%% API
-export([start_link/0,register_consumer/1,register_consumer/2,gen_consumer_node_info/1,register_provider/1,provider_watcher/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {zk_pid}).

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
    {ok,Pid} = connection(),
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

handle_call({add_consumer,Consumer}, _From, State) ->
    add_consumer(Consumer,State),
    {reply, ok, State};
handle_call({add_provider,Provider}, _From, State) ->
    register_provider_path(Provider,State),
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
handle_cast({provider_node_change,Interface,Path}, #state{zk_pid = Pid}=State) ->
    get_provider_and_start(Pid,Interface,Path),
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
    logger:info("zk server recv msg:~p",[_Info]),
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


register_consumer(Consumer) ->
    gen_server:call(?SERVER,{add_consumer,Consumer}),
    ok.
register_consumer(Name,Option)->
    Consumer=#consumer_config{interface = Name,methods = [<<"testa">>,<<"testb">>]},
    register_consumer(Consumer),
    ok.
register_provider(Provider)->
    gen_server:call(?SERVER,{add_provider,Provider}),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connection()->
    {ok,List} = application:get_env(dubboerl,zookeeper_list),
    {ok, Pid} = erlzk:connect(List, 30000, [
        {chroot, "/"},
        {monitor, self()}]),
    {ok,Pid}.

add_consumer(Consumer,State)->
    Pid= State#state.zk_pid,
%%    InterfacePath= << <<"/dubbo/">>/binary,Name/binary ,<<"consumers">>/binary >>,
    ConsumerNode = gen_consumer_node_info(Consumer),
    ConsumerNode2= list_to_binary(edoc_lib:escape_uri(binary_to_list(ConsumerNode))),
    check_and_create_path(Pid,<<"">>,[{<<"dubbo">>,p},{Consumer#consumer_config.interface,p},{<<"consumers">>,p},{ConsumerNode2,e} ]),
    get_provider_list(Consumer,State),
    ok.
register_provider_path(Provider,State)->
    #state{zk_pid = Pid}=State,
    ProviderNode = dubbo_node_config_util:gen_provider_info(Provider),
    check_and_create_path(Pid,<<"">>,[{<<"dubbo">>,p},{Provider#provider_config.interface,p},{<<"providers">>,p},{ProviderNode,e}]),
    ok.


get_provider_list(Consumer,State)->
    Pid= State#state.zk_pid,
    InterfacePath= << <<"/dubbo/">>/binary,(Consumer#consumer_config.interface)/binary ,<<"/providers">>/binary >>,
    get_provider_and_start(Pid,Consumer#consumer_config.interface,InterfacePath),
    ok.
get_provider_and_start(Pid,Interface,Path)->
    case erlzk:get_children(Pid,Path,spawn(dubbo_zookeeper,provider_watcher,[Interface])) of
        {ok,ChildList} ->
            logger:debug("get provider list ~p",[ChildList]),
            start_provider_process(Interface,ChildList),
            ok;
        {error,R1} ->
            logger:debug("[add_consumer] get_provider_list error ~p ~p",[R1]),
            ok
    end.

provider_watcher(Interface)->
    receive
        {node_children_changed,Path} ->
            gen_server:cast(?SERVER,{provider_node_change,Interface,Path}),
            logger:debug("provider_watcher get event ~p ~p",[node_children_changed,Path]);
        {Event, Path} ->
%%            Path = "/a",
%%            Event = node_created
            logger:debug("provider_watcher get event ~p ~p",[Event,Path])
    end,
    ok.


create_path(Pid,Path,CreateType)->
    case erlzk:create(Pid,Path,CreateType) of
        {ok,ActualPath}->
            logger:debug("[add_consumer] create zk path  success ~p",[ActualPath]),
            ok;
        {error,R1}->
            logger:debug("[add_consumer] create zk path error ~p ~p",[Path,R1])
    end,
    ok.
check_and_create_path(_Pid,_RootPath,[]) ->
    ok;
check_and_create_path(Pid,RootPath,[{Item,CreateType}|Rst])->
    CheckPath= << RootPath/binary,<<"/">>/binary,Item/binary >>,
    case erlzk:exists(Pid,CheckPath) of
        {ok,Stat} ->
            check_and_create_path(Pid,CheckPath,Rst);
        {error,no_node}->
            logger:debug("[add_consumer] check_and_create_path unexist no_node ~p",[CheckPath]),
            create_path(Pid,CheckPath,CreateType),
            check_and_create_path(Pid,CheckPath,Rst);
        {error,R1} ->
            logger:debug("[add_consumer] check_and_create_path unexist ~p",[R1]),
            check_and_create_path(Pid,CheckPath,Rst)
    end.

gen_consumer_node_info(Consumer)->
    %% revision参数字段的作用是什么？ 暂时不添加
    Methods=lists_util:join(Consumer#consumer_config.methods,<<",">>),
    Value=io_lib:format(<<"consumer://~s/~s?application=~s&category=~s&check=~p&default.timeout=~p&dubbo=~s&interface=~s&methods=~s&side=~s&timestamp=~p">>,
        [de_common_fun:local_ip_v4_str(),
            Consumer#consumer_config.interface,
            Consumer#consumer_config.application,
            Consumer#consumer_config.category,
            Consumer#consumer_config.check,
            Consumer#consumer_config.default_timeout,
            Consumer#consumer_config.dubbo_version,
            Consumer#consumer_config.interface,
            Methods,
            Consumer#consumer_config.side,
            time_util:timestamp_ms()
            ]),
    list_to_binary(Value).

%%dubbo_zookeeper:register_consumer(<<"com.ifcoder.abcd">>,[]).
start_provider_process(Interface,ProviderList)->
    dubbo_consumer_pool:start_consumer(Interface,ProviderList).

