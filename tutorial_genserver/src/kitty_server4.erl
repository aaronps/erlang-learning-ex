%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2016 8:06 PM
%%%-------------------------------------------------------------------
-module(kitty_server4).
-author("krom").
-behavior(gen_server).

%% API
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(cat, {name, color=green, description}).

start_link() -> gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% This is done very shitty, doesn't even check for errors...
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
    gen_server:call(Pid, terminate).


%%% Server functions

init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
    case Cats of
        [] ->
            {reply, make_cat(Name,Color,Description), Cats};

        [Cat | Rest] ->
            {reply, Cat, Rest}
    end;

handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat}, Cats) ->
    {noreply, [ Cat | Cats ]}.

handle_info(Msg, Cats) ->
    io:format("Some Info Message: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [ io:format("~p was set free.~n", [C#cat.name]) || C <- Cats ],
    ok.

code_change(OldVsn, State, Extra) ->
    io:format("code_change, OldVsn=~p, Extra=~p~n",[OldVsn, Extra]),
    {ok, State}.

%%% Private functions

make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.