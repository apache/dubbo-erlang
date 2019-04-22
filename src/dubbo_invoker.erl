%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2018 12:17 AM
%%%-------------------------------------------------------------------
-module(dubbo_invoker).
-author("dlive").

-include("dubbo.hrl").
%% API
-export([invoke_request/2,invoke_request/5]).

-spec invoke_request(Interface::binary(),Request::#dubbo_request{})->
    {ok,reference()}|
    {ok,reference(),Data::any(),RpcContent::list()}|
    {error,Reason::timeout|no_provider|any()}.
invoke_request(Interface,Request)->
    invoke_request(Interface,Request,[],#{},self()).

-spec invoke_request(Interface::binary(),Request::#dubbo_request{},RpcContext::list(),RequestState::map(),CallBackPid::pid())->
    {ok,reference()}|
    {ok,reference(),Data::any(),RpcContent::list()}|
    {error,Reason::timeout|no_provider|request_full|any()}.
invoke_request(Interface,Request,RpcContext,RequestState,CallBackPid)->
    case dubbo_consumer_pool:select_connection(Interface,Request#dubbo_request.mid) of
        {ok,#connection_info{pid=Pid,host_flag = HostFlag}} ->
            case dubbo_traffic_control:check_goon(HostFlag,199) of
                ok ->
                    Request2 = merge_attachments(Request,RpcContext),
                    {ok,RequestData} = de_codec:encode_request(Request2),
                    Ref=get_ref(RequestState),
                    RequestState2 = request_context:update(<<"t_agent_e">>,RequestState),
                    gen_server:cast(Pid,{send_request,Ref,Request2,RequestData,CallBackPid,RequestState2}),
                    case is_sync(RequestState) of
                        true->
                            sync_receive(Ref,get_timeout(RequestState));
                        false-> {ok, Ref}
                    end;
                full ->
                    {error,request_full}
            end;
        {error,none}->
            logger:error("[INVOKE] ~p error Reason no_provider",[Interface]),
            {error,no_provider};
        {error,R1}->
            logger:error("[INVOKE] ~p error Reason ~p",[Interface,R1]),
            {error,R1}
    end.


is_sync(Option)->
    maps:is_key(sync,Option).
%%    lists:member(sync,Option).
get_ref(Option)->
    maps:get(ref,Option,make_ref()).
%%    case maps:is_key(ref,Option) of
%%        true->
%%
%%    end,
%%    case proplists:get_value(ref,Option) of
%%        undefined->
%%            make_ref();
%%        Ref->
%%            Ref
%%    end.

get_timeout(Option)->
    maps:get(timeout,Option,?REQUEST_TIME_OUT).


sync_receive(Ref,TimeOut)->
    receive
        {'$gen_cast',{msg_back,Ref,Response,RpcContent}}->
            {ok,Ref,Response,RpcContent}
    after
        TimeOut ->
            {error,timeout}
    end.

merge_attachments(Request,Option)->
    Attachements= Request#dubbo_request.data#dubbo_rpc_invocation.attachments,
    case lists:keyfind(attachments,1,Option) of
        false->OptionAttachments=[];
        {attachments,OptionAttachments}->
            OptionAttachments
    end,
    List=[
        {<<"version">>, <<"0.0.0">>},
        {<<"timeout">>, <<"500">>}
    ],
    Attachements2= lists:merge3(Attachements,OptionAttachments,List),
    Data2=Request#dubbo_request.data#dubbo_rpc_invocation{attachments = Attachements2},
    Request#dubbo_request{data = Data2}.
