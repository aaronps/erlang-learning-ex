%%%
%%% Exercice 3-10: fill and justify text
%%%
-module(ex310).
-compile(export_all).

text() -> 
	"Write a function that will print this in a readable form, "
	"so that duplicates are removed and adjacent numbers are put into a "
	"range. You might like to think of doing this via a function which turns "
	"the earlier list of occurrences into a list like "
	"[{1,2},{4,6},{98,98},{100,100},{102,102}] "
	"through a sequence of transformations.".

test(1) -> fill(text(), 40);
test(2) -> justify(text(), 40).

fill(Text, MaxWidth) -> fill_line(Text, MaxWidth, 0).

fill_line(Text, MaxWidth, Pos) ->
	case read_word(Text) of
		{word, Word, Rest} ->
			WordLen = length(Word),
			NewPos = Pos + WordLen + 1,
			if 
				NewPos > MaxWidth -> io:format("~n~s",[Word]), fill_line(Rest, MaxWidth, WordLen);
				true -> write_word(Word, Pos), fill_line(Rest, MaxWidth, NewPos)
			end;
		nomore -> io:format("~n")
	end.
	
write_word(Word, 0) -> io:format("~s", [Word]);
write_word(Word, _) -> io:format(" ~s", [Word]).
	
read_word(Text) -> read_word(Text, []).

read_word([], []) 				-> nomore;
read_word([], Word) 			-> {word, lists:reverse(Word), []};
read_word([32 | Rest], []) 		-> read_word(Rest, []);
read_word([32 | Rest], Word) 	-> {word, lists:reverse(Word), Rest};
read_word([Char | Rest], Word) 	-> read_word(Rest, [ Char | Word ]).

justify(Text, MaxWidth) -> justify_line(Text, MaxWidth, 0, -1, []).

justify_line(Text, MaxWidth, CharCount, SpaceCount, Words) ->
	case read_word(Text) of
		{word, Word, Rest} ->
			WordLen = length(Word),
			NewCharCount = CharCount + WordLen,
			NewSpaceCount = SpaceCount + 1,
			NewSize = NewCharCount + NewSpaceCount,
			if 
				NewSize < MaxWidth ->
					justify_line(Rest, MaxWidth, NewCharCount, NewSpaceCount, [Word | Words]);
				NewSize > MaxWidth ->
					write_just(lists:reverse(Words), MaxWidth - (CharCount+SpaceCount)),
					justify_line(Rest, MaxWidth, WordLen, 0, [Word]);
				true ->
					write_just(lists:reverse([ Word | Words]), 0),
					justify_line(Rest, MaxWidth, 0, -1, [])
			end;
		nomore -> write_words(lists:reverse(Words), true)
	end.

write_words([], _) -> io:format("~n");
write_words([W | More], false) -> io:format(" ~s", [W]), write_words(More, false);
write_words([W | More], true) -> io:format("~s", [W]), write_words(More, false).


%% removed  and  adjacent numbers  are  put
%% into a range. You might like to think of
%% doing  this via  a function  which turns
%% the earlier  list of occurrences  into a

%% if there is only one, just print it
write_just([Word | []], _) -> io:format("~s~n", [Word]);

%% if there is no spaces, just print them.
write_just(Words, 0) ->
	write_words(Words, true);

%% if there is ExtraSpaces, get the extra space by real space
write_just(Words, ExtraSpaces) ->
	NumWords = length(Words),
	NumSpaces = NumWords - 1,
	ExtraRatio = ExtraSpaces / NumSpaces,
	%io:format("words:~w spaces:~w extra:~w = ~w~n", [NumWords, NumSpaces, ExtraSpaces, ExtraRatio]),
	write_just_extra(Words, {ExtraRatio, 0}).

%% if last word, print it and newline, finish
write_just_extra([Word | []], _) -> io:format("~s~n", [Word]);


write_just_extra([Word | More], SpaceState) ->
	io:format("~s ", [Word]),
	write_just_extra(More, write_spaces(SpaceState)).

write_spaces({Ratio, Acc}) ->
	NewAcc = Acc + Ratio,
	{ Ratio, write_space_x(NewAcc) }.

write_space_x(V) when V >= 0.5 -> io:format(" "), write_space_x(V - 1.0);
write_space_x(V) -> V.
	