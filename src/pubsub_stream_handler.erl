-module(pubsub_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state, {channel}).

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
    {Channel, _} = cowboy_req:binding(channel, Req),
    subscriber:subscribe(Channel),
	{ok, Req, #state{channel=Channel}}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, Req, State};
stream(_Data, Req, State) ->
	{ok, Req, State}.

info({message, _Channel, Mes, _Pid}, Req, State) ->
    io:format("Get message ~p~n", [Mes]),
    {reply, Mes, Req, State};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, State) ->
    subscriber:unsubscribe(State#state.channel),
	io:format("bullet terminate~n"),
	ok.
