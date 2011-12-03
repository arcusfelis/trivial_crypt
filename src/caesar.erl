%%% The realization of [Caesar cipher](http://en.wikipedia.org/wiki/Caesar_cipher)

-module(caesar).
-export([encode/2, decode/2
	]).


encode(S, N) -> 
	encode(S, N, []).

encode([H|T], N, Acc) 
	when H>=$a andalso H=<$z ->
	Diff = $z-$a,
	NewH = ((H - $a + N) rem Diff) + $a,
	encode(T, N, [NewH|Acc]);
encode([H|T], N, Acc) 
	when H>=$A andalso H=<$Z ->
	Diff = $Z-$A,
	NewH = ((H - $A + N) rem Diff) + $A,
	encode(T, N, [NewH|Acc]);
encode([H|T], N, Acc) ->
	encode(T, N, [H|Acc]);
encode([], _N, Acc) ->
	lists:reverse(Acc).

decode(S, N) -> 
	encode(S, -N, []).
