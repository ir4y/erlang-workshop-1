-module(pubsub_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).


init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	{ok, Req, state}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, Req, State};
stream(_Data, Req, State) ->
	{ok, Req, State}.

info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.