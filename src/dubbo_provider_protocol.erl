%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2018 7:19 PM
%%%-------------------------------------------------------------------
-module(dubbo_provider_protocol).
-author("dlive").

-behaviour(gen_server).
-behaviour(ranch_protocol).
-include("dubboerl.hrl").
-include("dubbo.hrl").


%% API
-export([start_link/4,register_impl_provider/3, select_impl_provider/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 5000).

-record(heartbeat,{last_write=0,last_read=0,timeout=50000,max_timeout=9000}).

-record(state, {transport,provider_config,socket =undefined,
    heartbeat=#heartbeat{},
    recv_buffer= <<>>          %%从客户端接收的数据
}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
%init([]) -> {ok, undefined}.

init({Ref, Socket, Transport, _Opts = []}) ->
    logger:info("provider connected"),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, transport=Transport},
        ?TIMEOUT).

handle_info({tcp,_Port,Data}, #state{recv_buffer = RecvBuffer,socket=Socket, transport=Transport} = State) ->
    Transport:setopts(Socket, [{active, once}]),
    NowBuffer = << RecvBuffer/binary,Data/binary >>,

    {ok,NextBuffer,NewState} = case check_recv_data(NowBuffer,State) of
                                   {next_buffer,<<>>,State2} ->
                                       {ok,<<>>,State2};
                                   {next_buffer,NextBuffer2,State3}->
                                       logger:debug("[INFO] recv one data state wait next_buffer"),
                                       {ok,NextBuffer2,State3}
                               end,
%%    HeartbeatInfo =update_heartbeat(write,NewState#state.heartbeat),
    {noreply, NewState#state{recv_buffer = NextBuffer}};
handle_info({tcp_closed,Port},State)->
%%    NewState=reconnect(State),
    {noreply, State};
%%handle_info({timeout, _TimerRef, {reconnect}},State)->
%%    NewState=reconnect(State),
%%    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_response,Data}, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket,Data) of
        ok ->
            ok;
        Other ->
            logger:warning("response error ~p",[Other])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


register_impl_provider(Interface,ImplModuleName,ModuleName)->
    ets:insert(?PROVIDER_IMPL_TABLE,{Interface,ImplModuleName,ModuleName}),
    ok.

-spec select_impl_provider(Interface::binary()) ->{ok,binary()} | {error,term()}.
select_impl_provider(Interface) ->
    case ets:lookup(?PROVIDER_IMPL_TABLE,Interface) of
        []->
            {error,no_provider};
        [{Interface,ImplModuleName,ModuleName}] ->
            {ok,ImplModuleName}
    end.



%%%=================================================================
%%% 接收数据处理
%%%=================================================================
-spec check_recv_data(Data::binary(),State::#state{})->{ready,ReadyData::binary()} | {ready,ReadyData::binary(),NextBuffer::binary()}.
check_recv_data(<<?DUBBO_MEGIC_HIGH,?DUBBO_MEGIC_LOW,Rest/binary>> = Data,State) when byte_size(Rest)<14 ->
    {next_buffer,Data,State};
check_recv_data(<<?DUBBO_MEGIC_HIGH,?DUBBO_MEGIC_LOW,_OtherFlag:80,DataLen:32,Rest/binary>> = Data,State) ->
    RestSize = byte_size(Rest),
    if
        DataLen==RestSize ->
            {ok,State2} = process_data(Data,State),
            {next_buffer,<<>>,State2};
        DataLen>RestSize ->
            logger:warning("need wait next buffer data ~p",[Data]),
            {next_buffer,Data,State};
        DataLen<RestSize ->
            <<ReadyData:DataLen/binary,NextBuffer/binary>> = Rest,
            OneData = <<?DUBBO_MEGIC_HIGH:8,?DUBBO_MEGIC_LOW:8,_OtherFlag:80,DataLen:32,ReadyData/binary>>,
            {ok,State3} = process_data(OneData,State),
            logger:warning("recevi more data ~w ",[NextBuffer]),
            check_recv_data(NextBuffer,State3)
    end;
check_recv_data(<<Error/integer,Data/binary>>,State)->
    logger:error("recv bad header data,Begin Byte:~p",[Error]),
    check_recv_data(Data,State);
check_recv_data(<<>>,State)->
    {next_buffer,<<>>,State}.


process_data(Data,State)->
    <<Header:16/binary,RestData/binary>> = Data,
    case dubbo_codec:decode_header(Header) of
        {ok,request,RequestInfo}->
            {ok,Req} = dubbo_codec:decode_request(RequestInfo,RestData),
            logger:info("get one request mid ~p, is_event ~p",[Req#dubbo_request.mid,Req#dubbo_request.is_event]),
            {ok,State2} = process_request(Req#dubbo_request.is_event,Req,State),
            {ok,State2};
        {ok,response,ResponseInfo}->
            {ok,Res} = dubbo_codec:decode_response(ResponseInfo,RestData),
            logger:info("get one response mid ~p, is_event ~p state ~p",[Res#dubbo_response.mid,Res#dubbo_response.is_event,Res#dubbo_response.state]),
            {ok,State3} =process_response(Res#dubbo_response.is_event,Res,State),
            {ok,State3};
        {error,Type,RelData}->
            logger:error("process_data error type ~p RelData ~p",[Type,RelData]),
            {ok,State}
    end.


%% @doc process event
-spec process_response(IsEvent::boolean(),#dubbo_response{},#state{})->ok.
process_response(true,Response,State)->

    {ok,State};

process_response(false,Response,State)->
%%    case get_earse_request_info(Response#dubbo_response.mid) of
%%        undefined->
%%            logger:error("dubbo response can't find request data,response ~p",[Response]);
%%        {SourcePid,Ref,Request} ->
%%            logger:debug("will cast mid ~p to source process SourcePid ~p",[Response#dubbo_response.mid,SourcePid]),
%%            RpcContent=[],
%%            ResponseData = de_type_transfer:response_to_native(Response),
%%            gen_server:cast(SourcePid,{msg_back,Ref,ResponseData,RpcContent})
%%    end,
    {ok,State}.

process_request(true,Request,State)->
%%    {ok,NewState} = send_heartbeat_msg(Request#dubbo_request.mid,State),
    logger:info("process request event ~p",[Request]),
    {ok,State};
process_request(false,Request,State)->
    logger:info("process request ~p",[Request]),
    dubbo_provider_worker:process_request(Request,self()),
    {ok,State}.