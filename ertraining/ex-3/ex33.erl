-module(ex33).
-export([print/1,print_even/1]).

print(N) -> print_n(1,N).

print_n(M,M) ->
	io:format("Number: ~p~n", [M]);
	
print_n(N,M) ->
	io:format("Number: ~p~n", [N]),
	print_n(N+1, M).
	
print_even(N) -> print_even_n(1, N).
	
	
print_even_n(M,M) when M rem 2 =:= 0 ->
	io:format("Number: ~p~n", [M]);

print_even_n(M,M) -> ok;
	
print_even_n(N, M) when N rem 2 =:= 0 ->
	io:format("Number: ~p~n", [N]),
	print_even_n(N+1,M);
	
print_even_n(N, M) -> print_even_n(N+1, M).