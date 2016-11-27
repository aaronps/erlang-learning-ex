-module(ex31).
-export([sum/1, sum/2]).

sum(N) when N > 0 -> sum_ac(N-1, N).

sum_ac(Number, Accumulator) when Number > 0 -> sum_ac(Number-1, Accumulator+Number);
sum_ac(_,Result) -> Result.


sum(N, M) when N =< M -> sum2_ac(N+1, M, N).

sum2_ac(N, M, S) when N =< M -> sum2_ac(N+1, M, S+N);
sum2_ac(_, _, S) -> S.