-module(ex32).
-export([create/1,reverse_create/1]).

create(N) when N > 0 -> create_ac(N, []).

create_ac(1, L) -> [ 1 | L ];
create_ac(N, L) -> create_ac(N-1, [N | L]).

reverse_create(N) when N > 0 -> reverse_create_ac(1, N, []).

reverse_create_ac(M, M, L) -> [ M | L];
reverse_create_ac(N, M, L) when N < M -> reverse_create_ac(N+1, M, [ N | L ]).
