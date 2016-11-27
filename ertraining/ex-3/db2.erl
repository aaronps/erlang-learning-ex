%%% exercice 3-7 db list with lists module
-module(db2).
-export([
	new/0,
	destroy/1,
	write/3,
	delete/2,
	read/2,
	match/2
]).

new() -> [].

destroy(_) -> ok.

write(Key, Value, Db) -> lists:keystore(Key, 1 ,Db, {Key, Value}).

delete(Key, Db) -> lists:keydelete(Key, 1, Db).

read(Key, Db) ->
	case lists:keyfind(Key, 1, Db) of
		false -> { error, instance};
		{Key, Value} -> {ok, Value}
	end.

match(Value, Db) -> 
	Fun = fun( {K,V}, Acc) ->
		case V of
			Value -> [ K | Acc];
			_ -> Acc
		end
	end,
	lists:foldl(Fun, [], Db).


