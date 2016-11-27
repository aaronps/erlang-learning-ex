%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 5:15 PM
%%%-------------------------------------------------------------------
-module(kitty_server2).
-author("krom").

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> spawn_link(fun init/0).

order_cat(Pid, Name, Color, Description) ->
    my_generic_server:call_naive(Pid, {order, Name, Color, Description}).

%% This is done very shitty, doesn't even check for errors...
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

close_shop(Pid) ->
    my_generic_server:call_naive(Pid, terminate).

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

