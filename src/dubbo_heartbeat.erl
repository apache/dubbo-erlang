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
-module(dubbo_heartbeat).

-include("dubbo.hrl").
%% API
-export([generate_request/2]).

-spec(generate_request(RequestId :: undefined|integer(), NeedResponse :: boolean()) -> {ok, binary()}).
generate_request(undefined, NeedResponse) ->
    RequestId = dubbo_id_generator:gen_id(),
    generate_request(RequestId, NeedResponse);
generate_request(RequestId, NeedResponse) ->
    Req = #dubbo_request{is_event = true, is_twoway = NeedResponse, mid = RequestId, data = undefined, mversion = <<"2.0.0">>},
    {ok, Bin} = dubbo_codec:encode_request(Req),
    {ok, Bin}.