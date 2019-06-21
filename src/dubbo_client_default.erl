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
-module(dubbo_client_default).

-behaviour(gen_server).


-include("dubbo.hrl").

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([start_link/1]).

-export([check_recv_data/2]).

-define(SERVER, ?MODULE).

-record(heartbeat, {last_write = 0, last_read = 0, timeout = 60000, max_timeout = 180000}).
-record(state, {provider_config, socket = undefined,
    heartbeat = #heartbeat{},
    recv_buffer = <<>>,         %%从服务端接收的数据
    host_flag,
    reconnection_timer,
    handler
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
-spec(start_link(Name :: binary(), ProviderConfig :: #provider_config{}) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ProviderConfig) ->
    gen_server:start_link(?MODULE, [ProviderConfig], []).

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
init([HostFlag, ProviderConfig]) ->
    #provider_config{host = Host, port = Port} = ProviderConfig,
    State = case open(Host, Port) of
                {ok, Socket} ->
                    #state{socket = Socket};
                {error, _Reason} ->
                    #state{}
            end,
    NowStamp = dubbo_time_util:timestamp_ms(),
    HeartBeatInfo = #heartbeat{last_read = NowStamp, last_write = NowStamp},
    logger:info("netty client start ~p", [HostFlag]),
    start_heartbeat_timer(HeartBeatInfo),
    {ok, State#state{provider_config = ProviderConfig, heartbeat = HeartBeatInfo, host_flag = HostFlag}}.

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

handle_cast({send_request, Ref, Request, Data, SourcePid, RequestState}, State) ->
    logger:debug("[send_request begin] send data to provider consumer mid ~p pid ~p sourcePid ~p", [Request#dubbo_request.mid, self(), SourcePid]),
    NewState = case send_msg(Data, State) of
                   ok ->
                       save_request_info(Request, SourcePid, Ref, RequestState),
                       logger:debug("[send_request end] send data to provider consumer pid ~p state ok", [self()]),
                       State;
                   {error, closed} ->
                       logger:warning("send request error, connection is closed"),
                       State2 = reconnect(State),
                       State2;
                   {error, R1} ->
                       logger:error("[send_request end] send data to provider consumer pid error ~p ~p", [self(), R1]),
                       State
               end,
    HeartbeatInfo = update_heartbeat(write, NewState#state.heartbeat),
    {noreply, NewState#state{heartbeat = HeartbeatInfo}};

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


handle_info({tcp, _Port, Data}, #state{recv_buffer = RecvBuffer} = State) ->
%%    inet:setopts(State#state.socket, [{active, once}]),
%%    logger:debug("[INFO] recv one data ~w",[Data]),
    {ok, NextBuffer, NewState} = case check_recv_data(<<RecvBuffer/binary, Data/binary>>, State) of
                                     {next_buffer, NextBuffer2, State3} ->
                                         logger:debug("[INFO] recv one data state wait next_buffer"),
                                         {ok, NextBuffer2, State3}
                                 end,
%%    HeartbeatInfo =update_heartbeat(write,NewState#state.heartbeat),
    {noreply, NewState#state{recv_buffer = NextBuffer}};
handle_info({tcp_closed, Port}, State) ->
    logger:info("dubbo connection closed ~p", [Port]),
    NewState = reconnect(State),
    {noreply, NewState};
handle_info({timeout, _TimerRef, {reconnect}}, State) ->
    NewState = reconnect(State#state{reconnection_timer = undefined}),
    {noreply, NewState};
handle_info({timeout, _TimerRef, {heartbeat_timer}}, State) ->
    {ok, NewState} = case check_heartbeat_state(State) of
                         {normal} -> {ok, State};
                         {send_heart} ->
                             send_heartbeat_msg(undefined, true, State);
                         {reconnect} ->
                             %% @todo reconnect
                             {ok, State}
                     end,
    HeartbeatInfo = update_heartbeat(write, NewState#state.heartbeat),
    start_heartbeat_timer(HeartbeatInfo),
    {noreply, NewState#state{heartbeat = HeartbeatInfo}};
handle_info(_Info, State) ->
    logger:warning("[INFO] get one info:~p", [_Info]),
%%    inet:setopts(State#state.socket, [{active, once}]),
%%    case State#state.tmp_pid of
%%        undefined  ->ok;
%%        Pid ->
%%            gen_server:cast(Pid,{msg_back})
%%    end,
    HeartbeatInfo = update_heartbeat(write, State#state.heartbeat),
    {noreply, State#state{heartbeat = HeartbeatInfo}}.

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
    logger:warning("terminate reason:~p", [_Reason]),
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

open(Host, Port) ->
    logger:debug("will connect to provider ~p ~p", [Host, Port]),
    %
    case gen_tcp:connect(Host, Port, [
        binary,
        {packet, 0}, {active, false},
        {reuseaddr, true},
        {delay_send, true},
        {nodelay, true},
        {high_watermark, 512 * 1024},
        {low_watermark, 256 * 1024},
        {sndbuf, 512 * 1024},
        {recbuf, 512 * 1024}
    ]) of
        {ok, Sockets} ->
            inet:setopts(Sockets, [{active, true}]),
            {ok, Sockets};
        Info ->
            logger:error("start client connection error ~p", [Info]),
            {error, Info}
    end.

reconnect(#state{reconnection_timer = Timer} = State) when Timer /= undefined ->
    State;
reconnect(State) ->
    #provider_config{host = Host, port = Port} = State#state.provider_config,
    case State#state.socket of
        undefined -> ok;
        Socket ->
            gen_tcp:close(Socket)
    end,
    case open(Host, Port) of
        {ok, Socket2} ->
            logger:warning("reconnect to provider ~p ~p success", [Host, Port]),
            State#state{socket = Socket2, recv_buffer = <<>>};
        {error, Reason} ->
            logger:warning("connect to provider error ~p", [Reason]),
            TimerRef = erlang:start_timer(2000, self(), {reconnect}),
            State#state{socket = undefined, reconnection_timer = TimerRef}
    end.

send_msg(Msg, State) ->
    case State#state.socket of
        undefined ->
            {error, closed};
        Socket ->
            case gen_tcp:send(Socket, Msg) of
                ok ->
                    ok;
                {error, Reason} ->
                    logger:error("send to server error,reason:~p", [Reason]),
                    {error, Reason}
            end
    end.

%%%=================================================================
%%% 心跳检测
%%%=================================================================
start_heartbeat_timer(HeartbeatInfo) ->
    erlang:start_timer(HeartbeatInfo#heartbeat.timeout, self(), {heartbeat_timer}),
    ok.
update_heartbeat(write, Info) ->
    Info#heartbeat{last_write = dubbo_time_util:timestamp_ms()};
update_heartbeat(read, Info) ->
    Info#heartbeat{last_read = dubbo_time_util:timestamp_ms()}.


check_heartbeat_state(#state{heartbeat = HeartBeatInfo} = _State) ->
    Now = dubbo_time_util:timestamp_ms(),
    #heartbeat{last_read = LastRead, last_write = LastWrite, timeout = Timeout, max_timeout = MaxTimeout} = HeartBeatInfo,
    if
        (Now - LastRead) > Timeout ->
            {send_heart};
        (Now - LastWrite) > Timeout ->
            {send_heart};
        (Now - LastRead) > MaxTimeout ->
            {reconnect};
        true ->
            {normal}
    end.


send_heartbeat_msg(Mid, NeedResponse, State) ->
    {ok, Bin} = dubbo_heartbeat:generate_request(Mid, NeedResponse),
    NewState = case send_msg(Bin, State) of
                   ok ->
                       logger:info("send one heartbeat to server"),
                       State;
                   {error, Reason} ->
                       logger:warning("dubbo connection send heartbeat error ~p", [Reason]),
                       State2 = reconnect(State),
                       State2
               end,
    {ok, NewState}.

%%%=================================================================
%%% 接收数据处理
%%%=================================================================
-spec check_recv_data(Data :: binary(), State :: #state{}) -> {ready, ReadyData :: binary()} | {ready, ReadyData :: binary(), NextBuffer :: binary()}.
check_recv_data(<<?DUBBO_MEGIC_HIGH, ?DUBBO_MEGIC_LOW, Rest/binary>> = Data, State) when byte_size(Rest) < 14 ->
    {next_buffer, Data, State};
check_recv_data(<<?DUBBO_MEGIC_HIGH, ?DUBBO_MEGIC_LOW, _OtherFlag:80, DataLen:32, Rest/binary>> = Data, State) ->
    RestSize = byte_size(Rest),
    if
        DataLen == RestSize ->
            {ok, State2} = process_data(Data, State),
            {next_buffer, <<>>, State2};
        DataLen > RestSize ->
            logger:warning("need wait next buffer data ~p", [Data]),
            {next_buffer, Data, State};
        DataLen < RestSize ->
            <<ReadyData:DataLen/binary, NextBuffer/binary>> = Rest,
            OneData = <<?DUBBO_MEGIC_HIGH:8, ?DUBBO_MEGIC_LOW:8, _OtherFlag:80, DataLen:32, ReadyData/binary>>,
            {ok, State3} = process_data(OneData, State),
%%            logger:warning("recevi more data ~w ",[NextBuffer]),
            check_recv_data(NextBuffer, State3)
    end;
check_recv_data(<<Error/integer, Data/binary>>, State) ->
    logger:error("recv bad header data,Begin Byte:~p", [Error]),
    check_recv_data(Data, State);
check_recv_data(<<>>, State) ->
    {next_buffer, <<>>, State}.


process_data(Data, State) ->
    <<Header:16/binary, RestData/binary>> = Data,
    case dubbo_codec:decode_header(Header) of
        {ok, response, ResponseInfo} ->
            process_response(ResponseInfo#dubbo_response.is_event, ResponseInfo, RestData, State),
%%            dubbo_traffic_control:decr_count(State#state.host_flag),
%%            case get_earse_request_info(ResponseInfo#dubbo_response.mid) of
%%                undefined->
%%                    logger:error("dubbo response can't find request data,response ~p",[ResponseInfo]);
%%                {SourcePid,Ref,_RequestState} ->
%%                    {ok,Res} = dubbo_codec:decode_response(ResponseInfo,RestData),
%%
%%                    logger:info("got one response mid ~p, is_event ~p state ~p",[Res#dubbo_response.mid,Res#dubbo_response.is_event,Res#dubbo_response.state]),
%%                    case Res#dubbo_response.is_event of
%%                        false ->
%%                            %% todo rpccontent need merge response with request
%%                            RpcContent=[],
%%                            ResponseData = dubbo_type_transfer:response_to_native(Res),
%%                            gen_server:cast(SourcePid,{response_process,Ref,RpcContent,ResponseData});
%%                        _->
%%                            ok
%%                    end
%%            end,
            {ok, State};
        {ok, request, RequestInfo} ->
            {ok, Req} = dubbo_codec:decode_request(RequestInfo, RestData),
            logger:info("get one request mid ~p, is_event ~p", [Req#dubbo_request.mid, Req#dubbo_request.is_event]),
            {ok, State2} = process_request(Req#dubbo_request.is_event, Req, State),
            {ok, State2};
        {error, Type, RelData} ->
            logger:error("process_data error type ~p RelData ~p", [Type, RelData]),
            {ok, State}
    end.


%% @doc process event
-spec process_response(IsEvent :: boolean(), #dubbo_response{}, #state{}, term()) -> ok.
process_response(false, ResponseInfo, RestData, State) ->
    dubbo_traffic_control:decr_count(State#state.host_flag),
    case get_earse_request_info(ResponseInfo#dubbo_response.mid) of
        undefined ->
            logger:error("dubbo response can't find request data,response ~p", [ResponseInfo]);
        {SourcePid, Ref, _RequestState} ->
            {ok, Res} = dubbo_codec:decode_response(ResponseInfo, RestData),
            logger:info("got one response mid ~p, is_event ~p state ~p", [Res#dubbo_response.mid, Res#dubbo_response.is_event, Res#dubbo_response.state]),
            case Res#dubbo_response.is_event of
                false ->
                    %% todo rpccontent need merge response with request
                    RpcContent = [],
                    ResponseData = dubbo_type_transfer:response_to_native(Res),
                    gen_server:cast(SourcePid, {response_process, Ref, RpcContent, ResponseData});
                _ ->
                    ok
            end
    end,
    {ok, State};
process_response(true, _ResponseInfo, _RestData, State) ->
    {ok, State}.

process_request(true, #dubbo_request{data = <<"R">>}, State) ->
    {ok, _} = dubbo_provider_consumer_reg_table:update_connection_readonly(self(), true),
    {ok, State};
process_request(true, Request, State) ->
    {ok, NewState} = send_heartbeat_msg(Request#dubbo_request.mid, false, State),
    {ok, NewState};
process_request(false, Request, State) ->
    {ok, State}.


save_request_info(Request, SourcePid, Ref, RequestState) ->
    put(Request#dubbo_request.mid, {SourcePid, Ref, RequestState}).
get_earse_request_info(Mid) ->
    erase(Mid).