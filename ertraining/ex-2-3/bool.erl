-module(bool).
-export([b_not/1]).


b_not(true) -> false;
b_not(false) -> true.

b_and(true, true) -> true;
b_and(false, _) -> false;
b_and(_, false) -> false.

b_or(true, true) -> true;
b_or(false, false) 