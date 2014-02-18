-module(pubsub_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-record(state, {subscribe}).

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
    {Channel, _} = cowboy_req:binding(channel, Req),
    {ok, Subscribe} = eredis_sub:start_link(),
    ok = eredis_sub:controlling_process(Subscribe, self()),
    ok = eredis_sub:subscribe(Subscribe, [Channel]),
	{ok, Req, #state{subscribe=Subscribe}}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, Req, State};
stream(_Data, Req, State) ->
	{ok, Req, State}.

info({subscribed, Channel, _Pid}, Req, State) ->
    io:format("Subscribed to ~p~n", [Channel]),
    eredis_sub:ack_message(State#state.subscribe),
    {ok, Req, State};
info({message, _Channel, Mes, _Pid}, Req, State) ->
    io:format("Get message ~p~n", [Mes]),
    eredis_sub:ack_message(State#state.subscribe),
    {reply, Mes, Req, State};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.
