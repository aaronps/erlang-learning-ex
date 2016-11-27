%%% This is for exercice 3-4, database handling USING LISTS
-module(db).
-export([
	new/0,
	destroy/1,
	write/3,
	delete/2,
	read/2,
	match/2
]).

%% creates a new DB, returns Db object
new() -> [].

%% Destroys the DB
destroy(_) -> ok.

%% Writes data to the DB, sets Key to value Element, returns new modified DB
write(Key, Element, Db) -> write_ac(Key, Element, Db, []).

write_ac(Key, Element, [], New) -> [{Key, Element} | New ]; 
write_ac(Key, Element, [ {Key, _} | Rest], New) -> [ {Key,Element} | Rest ++ New ];
write_ac(Key, Element, [Head | Rest], New) -> write_ac(Key, Element, Rest, [Head | New]).

%% Delete Key from DB, returns new modified db
delete(Key, Db) -> delete_ac(Key, Db, []).

delete_ac(_, [], New) -> New;
delete_ac(Key, [{Key,_} | Rest], New) -> Rest ++ New;
delete_ac(Key, [Head | Rest], New) -> delete_ac(Key, Rest, [Head | New]).

%% Read Value at Key, returns {ok, Element} or {error, instance}
read(Key, Db) -> read_ac(Key, Db).

read_ac(_, []) -> {error, instance};
read_ac(Key, [{Key,Element} | _]) -> {ok, Element};
read_ac(Key, [_ | Rest]) -> read_ac(Key, Rest).

%% Returns all keys that matches Element, returns list of keys
match(Element, Db) -> match_ac(Element, Db, []).

match_ac(_, [], Acc) -> Acc;
match_ac(Element, [ {Key, Element} | Rest ], Acc) -> match_ac(Element, Rest, [ Key | Acc]);
match_ac(Element, [ _ | Rest ], Acc) -> match_ac(Element, Rest, Acc).