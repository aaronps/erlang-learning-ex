-module(echo).
-export([go/0, loop/0]).

go() ->
	Pid = spawn(echo, loop, []),
	Pid ! { self(), hello },
	receive
		{Pid, Msg} -> io:format("GO: Received '~w'~n", [Msg])
	end,
	Pid ! stop.

loop() ->
	receive
		{From, Msg} ->
			io:format("LOOP: Received '~w' from go~n", [Msg]),
			From ! { self(), Msg },
			loop();
		stop -> true
	end.

