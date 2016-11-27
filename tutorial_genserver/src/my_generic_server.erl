%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 5:16 PM
%%%-------------------------------------------------------------------
-module(my_generic_server).
-author("krom").

%% API
-export([call_naive/2, start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public from here

call_naive(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! { self(), Ref, Msg },
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref,[flush]),
            Reply;

        %% The server died!
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)

    %% Didn't get anything... maybe server is bussy?
    after 5000 ->
        erlang:error(timeout)
    end.

start(Module, InitialState) ->
    spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! { sync, self(), Ref, Msg },
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref,[flush]),
            Reply;

    %% The server died!
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)

    %% Didn't get anything... maybe server is bussy?
    after 5000 ->
        erlang:error(timeout)
    end.

cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

%% This shall be used by the servers to send reply data,
%% they don't need to know what is the `From`.
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Private from here

init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} ->
            loop(Module, Module:handle_cast(Msg, State));

        {sync, Pid, Ref, Msg} ->
            loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.
