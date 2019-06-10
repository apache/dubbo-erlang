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
-module(dubbo_invoker_old).

-include("dubbo.hrl").
%% API
-export([invoke_request/2, invoke_request/3, invoke_request/5]).

-spec invoke_request(Interface :: binary(), Request :: #dubbo_request{}) ->
    {ok, reference()}|
    {ok, reference(), Data :: any(), RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
invoke_request(Interface, Request) ->
    invoke_request(Interface, Request, [], #{}, self()).

-spec invoke_request(Interface :: binary(), Request :: #dubbo_request{}, RequestOption :: map()) ->
    {ok, reference()}|
    {ok, reference(), Data :: any(), RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|any()}.
invoke_request(Interface, Request, RequestOption) ->
    invoke_request(Interface, Request, maps:get(ctx, RequestOption, []), RequestOption, self()).


-spec invoke_request(Interface :: binary(), Request :: #dubbo_request{}, RpcContext :: list(), RequestState :: map(), CallBackPid :: pid()) ->
    {ok, reference()}|
    {ok, reference(), Data :: any(), RpcContent :: list()}|
    {error, Reason :: timeout|no_provider|request_full|any()}.
invoke_request(Interface, Request, RpcContext, RequestState, CallBackPid) ->
    case dubbo_consumer_pool:select_connection(Interface, Request#dubbo_request.mid) of
        {ok, #connection_info{pid = Pid, host_flag = HostFlag}} ->
            case dubbo_traffic_control:check_goon(HostFlag, 199) of
                ok ->
                    Request2 = merge_attachments(Request, RpcContext),
                    {ok, RequestData} = dubbo_codec:encode_request(Request2),
                    Ref = get_ref(RequestState),
                    gen_server:cast(Pid, {send_request, Ref, Request2, RequestData, CallBackPid, RequestState}),
                    case is_sync(RequestState) of
                        true ->
                            sync_receive(Ref, get_timeout(RequestState));
                        false -> {ok, Ref}
                    end;
                full ->
                    {error, request_full}
            end;
        {error, none} ->
            logger:error("[INVOKE] ~p error Reason no_provider", [Interface]),
            {error, no_provider}
    end.


is_sync(Option) ->
    maps:is_key(sync, Option).
get_ref(Option) ->
    maps:get(ref, Option, make_ref()).

get_timeout(Option) ->
    maps:get(timeout, Option, ?REQUEST_TIME_OUT).


sync_receive(Ref, TimeOut) ->
    receive
        {'$gen_cast', {response_process, Ref, RpcContent, Response}} ->
            {ok, Ref, Response, RpcContent}
    after
        TimeOut ->
            {error, timeout}
    end.
merge_attachments(#dubbo_request{data = null} = Request, _Option) ->
    Request;
merge_attachments(Request, Option) ->
    Attachements = Request#dubbo_request.data#dubbo_rpc_invocation.attachments,
    case lists:keyfind(attachments, 1, Option) of
        false -> OptionAttachments = [];
        {attachments, OptionAttachments} ->
            OptionAttachments
    end,
    List = [
        {<<"version">>, <<"0.0.0">>},
        {<<"timeout">>, <<"5000">>}
    ],
    Attachements2 = lists:merge3(Attachements, OptionAttachments, List),
    Data2 = Request#dubbo_request.data#dubbo_rpc_invocation{attachments = Attachements2},
    Request#dubbo_request{data = Data2}.
