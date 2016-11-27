%%% exercice 3-6
-module(sorting).
-export([quicksort/1,mergesort/1,msort/2]).


quicksort([]) -> [];
quicksort([Pivot|List]) -> qsort(Pivot, List, [], []).

qsort(Pivot, [], Smaller, Bigger) -> quicksort(Smaller) ++ [Pivot] ++ quicksort(Bigger);

qsort(Pivot, [H|Rest], Smaller, Bigger) when H < Pivot ->
	qsort(Pivot, Rest, [ H | Smaller], Bigger);

qsort(Pivot, [H|Rest], Smaller, Bigger) ->
	qsort(Pivot, Rest, Smaller, [H | Bigger]).


mergesort(L) -> joinsort(makegroups(L), []).

makegroups([]) -> [];
makegroups([V|[]]) -> [V];
makegroups([A,B|L]) when A < B -> [[A,B] | makegroups(L)];
makegroups([A,B|L]) -> [[B,A] | makegroups(L) ].

joinsort([], [Acc|[]]) -> Acc;
joinsort([], Acc) -> joinsort(Acc, []);
joinsort([A,B|Rest], Acc) -> joinsort(Rest, [msort(A,B) | Acc]). 

msort([],Right) -> Right;
msort(Left, []) -> Left;
msort([Left | LRest], [Right | RRest]) when Left < Right -> [ Left | msort(LRest, [Right|RRest])];
msort(LRest, [Right | RRest]) -> [ Right | msort(LRest, RRest)];
msort([Left | LRest], Right ) when Left < Right -> [ Left | msort(LRest, Right)];
msort(Left, Right) -> [ Right | Left].
