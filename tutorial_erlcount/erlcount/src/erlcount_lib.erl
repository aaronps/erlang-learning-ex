%%%-------------------------------------------------------------------
%%% @author krom
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2016 10:57 PM
%%%-------------------------------------------------------------------
-module(erlcount_lib).
-author("krom").

%% API
-export([find_erl/1, regex_count/2]).

-include_lib("kernel/include/file.hrl").

find_erl(Directory) ->
    find_erl(Directory, queue:new()).

%%% @note doesn't have a return value because it is tail call
find_erl(Name, Queue) ->
    {ok, F= #file_info{}} = file:read_file_info(Name),
    case F#file_info.type of
        directory -> handle_directory(Name, Queue);
        regular -> handle_regular_file(Name, Queue);
        _Other -> dequeue_and_run(Queue)
    end.

handle_directory(Name, Queue) ->
    case file:list_dir(Name) of
        {ok, []} ->
            dequeue_and_run(Queue);
        {ok, Files} ->
            dequeue_and_run(enqueue_many(Name, Files, Queue))
    end.

dequeue_and_run(Queue) ->
    case queue:out(Queue) of
        {empty, _} -> done;
        {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

enqueue_many(Path, Files, Queue) ->
    QueueFun = fun(File, TempQueue) -> queue:in(filename:join(Path, File), TempQueue) end,
    lists:foldl(QueueFun, Queue, Files).

handle_regular_file(Name, Queue) ->
    case filename:extension(Name) of
        ".erl" ->
            {continue, Name, fun() -> dequeue_and_run(Queue) end};
        _NonErl ->
            dequeue_and_run(Queue)
    end.

regex_count(Regex, String) ->
    case re:run(String, Regex, [global]) of
        nomatch -> 0;
        {match, List} -> length(List)
    end.