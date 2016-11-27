%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 4:44 PM
%%%-------------------------------------------------------------------
-module(kitty_server).
-author("krom").

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

order_cat(Pid, Name, Color, Description) ->
    %% AHA!!! we use a monitor instead of a unique reference,
    %% because we also listen in case server dies!!!
    Ref = erlang:monitor(process, Pid),
    Pid ! { self(), Ref, {order, Name, Color, Description}},
    receive
        %% Got a CAT!!!
        {Ref, Cat} ->
            erlang:demonitor(Ref,[flush]),
            Cat;

        %% The server died!
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)

    %% Didn't get anything... maybe server is bussy?
    after 5000 ->
        erlang:error(timeout)
    end.

%% This is done very shitty, doesn't even check for errors...
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;

        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.


%%% Server functions

init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Descrioption}} ->
            case Cats of
                [] ->
                    Pid ! {Ref, make_cat(Name,Color,Descrioption)},
                    loop(Cats);

                [Cat | Rest] ->
                    Pid ! { Ref, Cat },
                    loop(Rest)
            end;

        {return, Cat = #cat{}} ->
            loop([Cat | Cats]);

        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);

        Unknown ->
            io:format("Some unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

%%% Private functions

make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

terminate(Cats) ->
    [ io:format("~p was set free.~n", [C#cat.name]) || C <- Cats ],
    ok.

