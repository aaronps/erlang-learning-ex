%%%
%%% Exercice 3-9: indexing
%%%
-module(idx).
-compile(export_all).

test() ->
	FileData = read("doc1.txt"),
	RawDoc = to_rawdoc(FileData),
	%Doc = to_doc(RawDoc), io:format("~p~n", [Doc]).
	Index = create_index(RawDoc),
	Index2 = lists:keymap(fun(V) -> lists:reverse(V) end, 2, Index),
	io:format("~w~n",[Index2]),
	print_readable(Index2).

%%
%% reads a file into a normal list
%%
read(Name) ->
	{ ok, Raw } = file:read_file(Name),
	binary_to_list(Raw).

%%
%% converts a normal list to a rawdoc
%%
to_rawdoc(L) -> to_rawdoc(L, []).

%%
%% Parses the data and generate the list of lines
%%
to_rawdoc([], Lines) -> lists:reverse(Lines);
to_rawdoc(Data, Lines) ->
	{line, Line, Rest} = to_line(Data, []),
	to_rawdoc(Rest, [Line | Lines]).

%%
%% Scans the line and returns {line, NewLine, RestOfData} when finds \r\n or []
%%
to_line([], Line) 			  -> {line, lists:reverse(Line), []};
to_line("\r\n" ++ Rest, Line) -> {line, lists:reverse(Line), Rest};
to_line([H|T], Line) 		  -> to_line(T, [H | Line]).

%%
%% From the RawDoc, generate a Doc, which is a list of words.
%%
to_doc(RawDoc) -> lists:reverse(to_doc(RawDoc, [])).

%%
%% Parses line by line the raw doc using to_words/2
%%
to_doc([], Doc) -> Doc;
to_doc([H|T], Doc) -> to_doc(T, to_words(H,[]) ++ Doc).

%%
%% Parses the line and gets word by word using to_word/2
%%
to_words([], Words) -> Words;
to_words(Data, Words) ->
	case to_word(Data, []) of
		{word, Word, Rest} -> to_words(Rest, [Word | Words]);
		lineend -> Words
	end.

%%
%% Gets characters until find (spaces, end of line, non-normal-characters)
%%
%% BUG: if a space is the last thing of the line, it will appear
to_word([], Word) ->
	case Word of
		[] -> lineend;
		_ -> {word, lists:reverse(Word), []}
	end;
to_word([32 | Rest], Word) -> 
	case Word of
		[] -> to_word(Rest, []);
		_ -> {word, lists:reverse(Word), Rest}
	end;

to_word("i.e." ++ Rest, _) -> { word, "i.e.", Rest };
	
to_word([H|T], Word) when H >= $A, H =< $Z; H >= $a, H =< $z -> to_word(T, [H | Word]);
to_word([_|Rest], Word) ->
	case Word of
		[] -> to_word(Rest, []);
		_ -> {word, lists:reverse(Word), Rest}
	end.

%%
%% Does the same as to_doc/1 but returns a index.
%%
create_index(RawDoc) -> to_index(RawDoc, idxdb:new(), 1).

to_index([], DB, _) -> DB;
to_index([Line | Rest], DB, Para) -> to_index(Rest, to_line_index(Line, DB, Para), Para+1).

to_line_index([], DB, _) -> DB;
to_line_index(Data, DB, Para) ->
	case to_word(Data, []) of
		{word, Word, Rest} -> to_line_index(Rest, idxdb:insert_or_update(DB, Word, Para), Para);
		lineend -> DB
	end.

%%
%% print readable index
%%
print_readable([]) -> ok;
print_readable([{Word, Indexes} | Rest]) ->
	print_word(Word, Indexes),
	print_readable(Rest).

print_word(Word, Indexes) ->
	io:format("  ~.10s", [Word]),
	print_indexes(Indexes).

print_indexes(Indexes) -> print_index(Indexes, -1).

print_index([], Para) 			-> io:format("~w~n", [Para]);

print_index([Para|Rest], Para)  -> print_index(Rest, Para);
print_index([Para|Rest], Para2) when Para =:= Para2+1 ->
	io:format("~w-", [Para2]),
	print_continuous_index(Rest, Para);
print_index([Para|Rest], -1) 	-> print_index(Rest, Para);
print_index([Para|Rest], Para2) -> io:format("~w,", [Para2]), print_index(Rest, Para).

print_continuous_index([], End) 		    -> io:format("~w~n", [End]);
print_continuous_index([Para|Rest], Para)   -> print_continuous_index(Rest, Para);
print_continuous_index([Para|Rest], Para2) when Para =:= Para2+1 -> print_continuous_index(Rest, Para);
print_continuous_index([Para|Rest], Para2) -> io:format("~w,",[Para2]), print_index(Rest, Para).
	



