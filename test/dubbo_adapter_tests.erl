%%%-------------------------------------------------------------------
%%% @author dlive
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 00:34
%%%-------------------------------------------------------------------
-module(dubbo_adapter_tests).
-author("dlive").
-include("dubbo.hrl").
-include_lib("eunit/include/eunit.hrl").

reference_test() ->
  dubbo_id_generator:start_link(),
  Invocation = #dubbo_rpc_invocation{
    className = <<"testname">>,
    classVersion = <<"testversion">>,
    methodName = <<"getUserInfo">>,
    parameterDesc = <<"Ljava/lang/String;"/utf8>>,
    parameterTypes = [
      #type_def{foreign_type = <<"java.lang.String">>,
        native_type = string,
        fieldnames = []}
    ],
    parameters = [
      <<"test">>
    ],
    attachments = [
      {<<"path">>, <<"testname">>},
      {<<"interface">> , <<"testname">>}
    ]
  },
  Request = dubbo_adapter:reference(Invocation),
  ?assert(is_record(Request,dubbo_request)).
