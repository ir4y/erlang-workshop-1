-module(redisproxy).

-export([
    start/0,
    stop/0
    ]).

-define(APPS, [crypto, ranch, cowlib, cowboy, redisproxy]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    ok = ensure_started(?APPS),
    ok = sync:go().

stop() ->
    sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).
