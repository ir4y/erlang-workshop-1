-module(redisproxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    subscriber:start_link(),
    Dispatch = cowboy_router:compile([
                {'_', [
                        {"/pubsub/:channel", bullet_handler, [{handler, pubsub_stream_handler}]},
                        {"/static/[...]", cowboy_static, {dir,"priv"}}
                    ]}
                ]),
    Ip = {0, 0, 0, 0},
    Port = 8008,
    {ok, _} = cowboy:start_http(
            http, 100, [{ip, Ip}, {port, Port}], [{env, [{dispatch, Dispatch}]}]),
    redisproxy_sup:start_link().
stop(_State) ->
    ok.
