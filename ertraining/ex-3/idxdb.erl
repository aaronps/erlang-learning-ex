%%%
%%% A module to handle the indexes of exercise 3-9
%%%
-module(idxdb).
-compile(export_all).

new() -> [].

insert_or_update(DB, Key, Value) ->
	case lists:keyfind(Key, 1, DB) of
		false -> lists:keystore(Key, 1 , DB, {Key, [Value]});
		{Key, OldValue} -> lists:keystore(Key, 1 , DB, {Key, [Value|OldValue]})
	end.


