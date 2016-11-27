%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 5:28 PM
%%%-------------------------------------------------------------------
-module(kitty_server3).
-author("krom").

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).


-record(cat, {name, color=green, description}).

%%% Client API
start_link() -> my_generic_server:start_link(?MODULE, []).

order_cat(Pid, Name, Color, Description) ->
    my_generic_server:call(Pid, {order, Name, Color, Description}).

%% This is done very shitty, doesn't even check for errors...
return_cat(Pid, Cat = #cat{}) ->
    my_generic_server:cast(Pid, {return, Cat}),
    ok.

close_shop(Pid) ->
    my_generic_server:call(Pid, terminate).

%%% Server functions

init([]) -> [].

handle_call({order, Name, Color, Description}, From, Cats) ->
    case Cats of
        [] ->
            my_generic_server:reply(From, make_cat(Name,Color,Description)),
            Cats;

        [Cat | Rest] ->
            my_generic_server:reply(From, Cat),
            Rest
    end;

handle_call(terminate, From, Cats) ->
    my_generic_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat}, Cats) ->
    [ Cat | Cats ].

%%% Private functions

make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

terminate(Cats) ->
    [ io:format("~p was set free.~n", [C#cat.name]) || C <- Cats ],
    exit(normal).

