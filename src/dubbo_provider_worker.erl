%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2018 8:08 PM
%%%-------------------------------------------------------------------
-module(dubbo_provider_worker).
-author("dlive").

-behaviour(gen_server).

%% API
-export([start_link/1,process_request/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-include("dubbo.hrl").
-include("dubboerl.hrl").
-include("dubbo_type.hrl").

-record(heartbeat,{last_write=0,last_read=0,timeout=50000,max_timeout=9000}).
-record(state, {provider_config,socket =undefined,
    heartbeat=#heartbeat{},
    recv_buffer= <<>>          %%从客户端接收的数据
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

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
init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_cast({request,Request,SourcePid},State)->
%%    #dubbo_request{mid=Mid} = Request,

%%    Data = #databaseOperateResponse{databaseOperateRsp = "ha-ha"},
%%    Data2 =#dubbo_rpc_invocation{parameters = [Data]},
%%    {ok,Content }= de_codec:encode_response(#dubbo_response{mid=Mid,is_event = false,data= Data2}),
    {ok,Content} = invoker_implement(Request),
    gen_server:cast(SourcePid,{send_response,Content}),

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


process_request(Request,SourcePid)->
    Worker = poolboy:checkout(?PROVIDER_WORKER),
    try
        gen_server:cast(Worker,{request,Request,SourcePid})
    after
        ok = poolboy:checkin(?PROVIDER_WORKER, Worker)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec invoker_implement(#dubbo_request{})-> {ok,response_content()}.
invoker_implement(Request)->
    #dubbo_rpc_invocation{className = Interface,methodName = MethodName,parameters = Parameters} = Request#dubbo_request.data,
    case dubbo_provider_protocol:select_impl_provider(Interface) of
        {ok,ImplModule}->
            case apply(ImplModule,binary_to_atom(MethodName,latin1),Parameters) of
                {error}->
                    ok;
                #dubbo_rpc_invocation{}=ResultInvoca ->
                    #dubbo_request{mid = Mid} = Request,
                    {ok,Content }= de_codec:encode_response(#dubbo_response{mid=Mid,is_event = false,data= ResultInvoca}),
                    {ok,Content};
                ResultObj->
%%                    Data = #databaseOperateResponse{databaseOperateRsp = "ha-ha"},
                    #dubbo_request{mid = Mid} = Request,
                    Data2 =#dubbo_rpc_invocation{parameters = [ResultObj]},
                    {ok,Content }= de_codec:encode_response(#dubbo_response{mid=Mid,is_event = false,data= Data2}),
                    {ok,Content}
            end;
        {error,Reason}  ->
            {error,Reason}
    end.