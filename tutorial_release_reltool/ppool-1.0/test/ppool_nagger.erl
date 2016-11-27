%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2016 10:55 AM
%%%-------------------------------------------------------------------
-module(ppool_nagger).
-author("krom").
-behavior(gen_server).

%% API
-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(NagMessage, DelayBetweenSends, MaxSends, DestPidOrName) ->
    gen_server:start_link(?MODULE, {NagMessage, DelayBetweenSends, MaxSends, DestPidOrName}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({_NagMessage, DelayBetweenSends, _MaxSends, _DestPidOrName} = Params) ->
    { ok, Params, DelayBetweenSends}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, {NagMessage, DelayBetweenSend, MaxSends, DestPidOrName} = State) ->
    DestPidOrName ! { self(), NagMessage },
    if
        MaxSends =:= infinity ->
            {noreply, State, DelayBetweenSend};

        MaxSends =< 1 ->
            {stop, normal, {NagMessage, DelayBetweenSend, 0, DestPidOrName}};

        MaxSends > 1 ->
            {noreply, {NagMessage, DelayBetweenSend, MaxSends-1, DestPidOrName}, DelayBetweenSend}
    end.
%% NOT HANDLING MORE INFO, LET IT DIE IF SOMETHING ELSE HAPPENS

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
