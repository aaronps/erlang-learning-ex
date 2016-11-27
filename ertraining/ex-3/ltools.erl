%%% exercice 3-5
-module(ltools).
-export([
	filter/2,filter2/2,
	reverse/1,reverse2/1,reverse3/1,
	concatenate/1,
	flatten/1
]).

%% take list and a number, returns the values of list less or equal to number
filter([],_) -> [];
filter([H|T],N) when H =< N -> [H | filter(T,N)];
filter([_|T],N) -> filter(T,N).

filter2(L, N) -> filter2_ac(L, N, []).

filter2_ac([], _, ACC) -> reverse(ACC);
filter2_ac([H|Rest], N, ACC) when H =< N -> filter2_ac(Rest, N, [ H | ACC]);
filter2_ac([_|Rest], N, ACC) -> filter2_ac(Rest, N, ACC).

%% reverse list
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

reverse2([]) -> [];
reverse2([H|T]) -> reverse2_ac(T, [H]).

reverse2_ac([], Acc) -> Acc;
reverse2_ac([H|T], Acc) -> reverse2_ac(T, [ H | Acc]).

reverse3(L) -> reverse2_ac(L, []).

%% give a list of lists, concatenate them
concatenate([]) -> [];
concatenate([H|T]) -> copy(H) ++ concatenate(T).

copy([]) -> [];
copy([H|T]) -> [ H | copy(T)].

%% give a list of nested lists, flatten to single list.
flatten([]) -> [];
flatten([H|T]) when is_list(H) -> flatten(H) ++ flatten(T);
flatten([H|T]) -> [H | flatten(T)].

