%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2019 11:06
%%%-------------------------------------------------------------------
-module(dubbo_service_user_impl).
-author("dlive").

-behaviour(user2).

-include_lib("dubbo_service.hrl").
-include_lib("dubboerl/include/hessian.hrl").
-include_lib("dubboerl/include/dubbo.hrl").
%% API
-export([getUserInfo/1,queryUserList/1,genUserId/0,queryUserInfo/1]).

genUserId()->
  "newid".

getUserInfo(Args) ->
  io:format(user,"do invokeWs ~p",[Args]),
  #userInfo{userAge = 88,userName = "one", userId = "id123"}.

queryUserList(Args)->
  User = #userInfo{userAge = 88,userName = "two", userId = "id123"},
  List = #list{len=1,type="java.util.ArrayList",values=[User]},

  Res = #userRes{
    userlist = List
  },
  Res.


queryUserInfo(Arg0)->
  io:format(user,"do invoker queryUserInfo ~p",[Arg0]),
  #userInfo{userName = "uuname",userAge = 10,userId = "44"}.