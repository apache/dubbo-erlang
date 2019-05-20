%%%-------------------------------------------------------------------
%% @doc dubboerl_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(dubboerl_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1,test_fun/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_app(),
    dubboerl:init(),
    start_web(),
    dubboerl_demo_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_app()->
    application:ensure_all_started(dubboerl),
    application:ensure_all_started(dubbo_sample_service),
    application:ensure_all_started(cowboy),
    ok.

start_web()->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", api_gateway_handle, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port,9090}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.

test_fun()->
        ok1.