%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Feb 2018 10:57 PM
%%%-------------------------------------------------------------------
-module(api_gateway_handle).
-author("dlive").

-include_lib("dubbo_sample_service/include/dubbo_sample_service.hrl").

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).
-export([info/3]).

%%init(Req, Opts) ->
%%    {cowboy_rest, Req, Opts}.
init(Req, State) ->
    io:format("get loop init ~n"),
    request_to_dubbo(Req,State),
    {cowboy_loop, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json},
        {<<"text/plain">>, hello_to_text}
    ], Req, State}.

hello_to_html(Req, State) ->
    Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>">>,
    {Body, Req, State}.

hello_to_json(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

hello_to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.


info({reply, Body}, Req, State) ->
    cowboy_req:reply(200, #{}, Body, Req),
    {stop, Req, State};
info({'$gen_cast',{msg_back,Ref,Response,RpcContent}},Req,State)->
    io:format("get msg_back ~p~n",[Response]),
    Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>">>,
    Req2=cowboy_req:reply(200, #{}, Body, Req),
    {stop, Req2, State};
info(_Msg, Req, State) ->
    io:format("get info ~p~n",[_Msg]),
    {ok, Req, State, hibernate}.

request_to_dubbo(Req, State)->
    userOperator:queryUserInfo(#userInfoRequest{username = "name",requestId = "111"},#{sync=> true}),
    ok.
