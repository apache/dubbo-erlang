-module(userOperator).

-include_lib("dubboerl/include/dubbo.hrl").
-include_lib("dubboerl/include/hessian.hrl").

-define(CURRENT_CLASS_NAME,<<"org.apache.dubbo.erlang.sample.service.facade.UserOperator"/utf8>>).
-define(CURRENT_CLASS_VERSION,<<"0.0.0"/utf8>>).

-include("dubbo_sample_service.hrl").




%% API
-export([
    getUserInfo/1,
    getUserInfo/2,
    genUserId/0,
    genUserId/1,
    queryUserInfo/1,
    queryUserInfo/2,
    queryUserList/1,
    queryUserList/2]).

-export([get_method_999_list/0]).

%% behaviour
-callback getUserInfo(Arg0::list())-> #userInfo{}.
-callback genUserId()-> list().
-callback queryUserInfo(Arg0::#userInfoRequest{})-> #userInfo{}.
-callback queryUserList(Arg0::list())-> #userRes{}.

get_method_999_list()->
    [
    getUserInfo,
    genUserId,
    queryUserInfo,
    queryUserList].



-spec getUserInfo(Arg0::list())->
    {ok,reference()}|
    {ok,reference(),Data::#userInfo{},RpcContent::list()}|
    {error,Reason::timeout|no_provider|any()}.
getUserInfo(Arg0)->
    getUserInfo(Arg0 ,#{}).

getUserInfo(Arg0, RequestOption)->
    
    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"getUserInfo">>,
        parameterDesc = <<"Ljava/lang/String;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"java.lang.String">>,
            native_type = string,
            fieldnames = []}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">> , ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME,Request,RequestOption).


-spec genUserId()->
    {ok,reference()}|
    {ok,reference(),Data::list(),RpcContent::list()}|
    {error,Reason::timeout|no_provider|any()}.
genUserId()->
    genUserId( #{}).

genUserId( RequestOption)->
    
    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"genUserId">>,
        parameterDesc = <<""/utf8>>,
        parameterTypes = [
            
        ],
        parameters = [
            
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">> , ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME,Request,RequestOption).


-spec queryUserInfo(Arg0::#userInfoRequest{})->
    {ok,reference()}|
    {ok,reference(),Data::#userInfo{},RpcContent::list()}|
    {error,Reason::timeout|no_provider|any()}.
queryUserInfo(Arg0)->
    queryUserInfo(Arg0 ,#{}).

queryUserInfo(Arg0, RequestOption)->
    
    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"queryUserInfo">>,
        parameterDesc = <<"Lorg/apache/dubbo/erlang/sample/service/bean/UserInfoRequest;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"org.apache.dubbo.erlang.sample.service.bean.UserInfoRequest">>,
            native_type = userInfoRequest,
            fieldnames = record_info(fields,userInfoRequest)}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">> , ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME,Request,RequestOption).


-spec queryUserList(Arg0::list())->
    {ok,reference()}|
    {ok,reference(),Data::#userRes{},RpcContent::list()}|
    {error,Reason::timeout|no_provider|any()}.
queryUserList(Arg0)->
    queryUserList(Arg0 ,#{}).

queryUserList(Arg0, RequestOption)->
    
    Data = #dubbo_rpc_invocation{
        className = ?CURRENT_CLASS_NAME,
        classVersion = ?CURRENT_CLASS_VERSION,
        methodName = <<"queryUserList">>,
        parameterDesc = <<"Ljava/lang/String;"/utf8>>,
        parameterTypes = [
            #type_def{foreign_type = <<"java.lang.String">>,
            native_type = string,
            fieldnames = []}
        ],
        parameters = [
            Arg0
        ],
        attachments = [
            {<<"path">>, ?CURRENT_CLASS_NAME},
            {<<"interface">> , ?CURRENT_CLASS_NAME}
        ]
    },
    Request = dubbo_adapter:reference(Data),
    dubbo_invoker:invoke_request(?CURRENT_CLASS_NAME,Request,RequestOption).

