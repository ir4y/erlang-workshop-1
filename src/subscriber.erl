-module(subscriber).

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribe, clients}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(Channel) ->
    gen_server:call(?MODULE, {subscribe, Channel}).

unsubscribe(Channel) ->
    gen_server:call(?MODULE, {unsubscribe, Channel}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, Subscribe} = eredis_sub:start_link(),
    ok = eredis_sub:controlling_process(Subscribe, self()),
    {ok, #state{subscribe=Subscribe, clients=[]}}.

handle_call({subscribe, Channel}, From, State) ->
    io:format("Subscribed to ~p~n", [Channel]),
    case erlang:length([SubChannel || {SubChannel, _} <- State#state.clients, Channel =:= SubChannel]) of
        0 -> ok = eredis_sub:subscribe(State#state.subscribe, [Channel]);
        _ -> ok
    end,
    {reply, ok, State#state{clients=[{Channel, From}] ++ State#state.clients}};
handle_call({unsubscribe, Channel}, From, State) ->
    io:format("Unsubscribed to ~p~n", [Channel]),
    case erlang:length([SubChannel || {SubChannel, _} <- State#state.clients, Channel =:= SubChannel]) of
        0 -> ok = eredis_sub:unsubscribe(State#state.subscribe, [Channel]);
        _ -> ok
    end,
    {reply, ok, State#state{clients=State#state.clients -- [{Channel, From}]}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({subscribed, _Channel, _Pid}, State) ->
    eredis_sub:ack_message(State#state.subscribe),
    {noreply, State};
handle_info(Message={message, Channel, _Mes, _Pid}, State) ->
    [Pid ! Message || {SubChannel, {Pid, _Ref}} <- State#state.clients, Channel =:= SubChannel],
    eredis_sub:ack_message(State#state.subscribe),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
