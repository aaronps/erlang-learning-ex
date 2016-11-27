%%% exercice 3-8 calculator language
-module(calclang).
-export([parse/1,prettyprint/1, evalue/1, compile/1, simulate/1, simplify/1, lexer/2]).

%% returns the parse tree
parse(Str) -> parser(lexer(Str, [])).

%%
%% lexer -> from the source string, generates the token tree
%%
lexer([], Tree) -> lists:reverse(Tree);

lexer([$) | Rest], Tree) -> {{subexpression, lists:reverse(Tree)}, Rest};

lexer([$( | Rest], Tree) ->
	{E, More} = lexer(Rest, []),
	lexer(More, [E | Tree]);

lexer([$+ | Rest], Tree) -> lexer(Rest, [{operator, plus} | Tree]);
lexer([$- | Rest], Tree) -> lexer(Rest, [{operator, minus} | Tree]);
lexer([$* | Rest], Tree) -> lexer(Rest, [{operator, multiply} | Tree]);
lexer([$/ | Rest], Tree) -> lexer(Rest, [{operator, divide} | Tree]);
lexer([$~ | Rest], Tree) -> lexer(Rest, [{operator, negate} | Tree]);

lexer([V | Rest], Tree) when V >= $0, V =< $9 ->
	case Tree of
		[{num, PrevValue}|TreeRest] -> lexer(Rest, [{num, PrevValue*10 + V-$0} | TreeRest]);
		_ -> lexer(Rest, [{num,V - $0} | Tree])
	end;

lexer ( "if " ++ Rest, Tree)    -> lexer(Rest, [{condition, c_if} | Tree]);
lexer ( " then " ++ Rest, Tree) -> lexer(Rest, [{condition, c_then} | Tree]);
lexer ( " else " ++ Rest, Tree) -> lexer(Rest, [{condition, c_else} | Tree]).

%%
%% parser -> from the lexter tree, generates the required structure
%%
parser({num, Value}) -> {num, Value};
parser({subexpression, S}) -> parser(S);

parser([{num, Value}]) -> {num, Value};
parser([{subexpression, S}]) -> parser(S);
	
parser([{operator, negate}, S]) -> { negate, parser(S) };

parser([A, {operator, OP}, B]) -> {OP, parser(A), parser(B)};

parser([{condition, c_if}, A, {condition, c_then}, B, {condition,c_else}, C]) ->
	{ c_if, parser(A), c_then, parser(B), c_else, parser(C) }.

%%
%% the pretty printer
%%
prettyprint(L) ->
	pprint(L),
	io:format("~n").
	
pprint([]) -> ok;

pprint({num, Value}) 	 -> io:format("~p",[Value]);
pprint({plus, A, B}) 	 -> io:format("("), pprint(A), io:format("+"), pprint(B), io:format(")");
pprint({minus, A, B}) 	 -> io:format("("), pprint(A), io:format("-"), pprint(B), io:format(")");
pprint({multiply, A, B}) -> io:format("("), pprint(A), io:format("*"), pprint(B), io:format(")");
pprint({divide, A, B}) 	 -> io:format("("), pprint(A), io:format("/"), pprint(B), io:format(")");
pprint({negate, A}) 	 -> io:format("~~"), pprint(A);

pprint({c_if, A, c_then, B, c_else, C}) ->
	io:format("if "), pprint(A), io:format(" then "), pprint(B), io:format(" else "), pprint(C);

pprint([ H | T ]) -> pprint(H), pprint(T).

%%
%% evaluate the result
%%
evalue([]) -> 0;

evalue({num, Value}) 	 -> Value;
evalue({plus, A, B}) 	 -> evalue(A) + evalue(B);
evalue({minus, A, B}) 	 -> evalue(A) - evalue(B);
evalue({multiply, A, B}) -> evalue(A) * evalue(B);
evalue({divide, A, B}) 	 -> evalue(A) / evalue(B);
evalue({negate, A}) 	 -> -evalue(A);

evalue({c_if, A, c_then, B, c_else, C}) ->
	case evalue(A) of
		0 -> evalue(B);
		_ -> evalue(C)
	end;

evalue([ H | T ]) -> evalue(H) + evalue(T).

%%
%% compile to simulated machine code
%%
compile(Tree) -> lists:flatten(compile_exp(Tree)).

compile_exp({num, Value}) -> {push, Value};
compile_exp({plus, A, B}) -> [compile_exp(A), compile_exp(B), add];
compile_exp({minus, A, B}) -> [compile_exp(A), compile_exp(B), sub];
compile_exp({multiply, A, B}) -> [compile_exp(A), compile_exp(B), mul];
compile_exp({divide, A, B}) -> [compile_exp(A), compile_exp(B), divi];
compile_exp({negate, A}) -> [compile_exp(A), negate];

compile_exp({c_if, A, c_then, B, c_else, C}) ->
	[compile_exp(B), compile_exp(C), compile_exp(A), opif ].

%%
%% Simulator of the opcodes
%%
simulate(Code) -> run(Code, []).

run([], Stack) -> Stack;

run({push, Value}, Stack) -> [ Value | Stack ];
run(add, [A,B|Rest]) -> [ A+B | Rest ]; 
run(sub, [A,B|Rest]) -> [ B-A | Rest ];
run(mul, [A,B|Rest]) -> [ A*B | Rest ];
run(divi, [A,B|Rest]) -> [ B/A | Rest ];
run(negate, [A|Rest]) -> [ -A | Rest ];

run(opif, [A, C, B | Rest]) ->
	case A of
		0 -> [ B | Rest ];
		_ -> [ C | Rest ]
	end;

run([H|T], Stack) -> run(T, run(H, Stack)).

%%
%% simplify
%%

simplify({num, X}) -> {num, X};

simplify({OP, A, B}) when OP == plus; OP == minus -> 
	if
		A == {num, 0} -> simplify(B);
		B == {num, 0} -> simplify(A);
		true -> {OP, simplify(A), simplify(B)}
	end;

simplify({multiply, A, B}) ->
	if
		A == {num, 1} -> simplify(B);
		B == {num, 1} -> simplify(A);
		true -> {multiply, simplify(A), simplify(B)}
	end;

simplify({divide, A, B}) ->
	if
		B == {num, 1} -> simplify(A);
		true -> {divide, simplify(A), simplify(B)}
	end;
	
simplify({negate, V}) -> {negate, simplify(V)};

simplify({c_if, A, c_then, B, c_else, C}) ->
	case A of
		{num, 0} -> simplify(B);
		{num, _} -> simplify(C);
		_ -> {c_if, simplify(A), c_then, simplify(B), c_else, simplify(C)}
	end.