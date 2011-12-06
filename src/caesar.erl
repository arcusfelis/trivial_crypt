%%% The realization of [Caesar cipher](http://en.wikipedia.org/wiki/Caesar_cipher)

-module(caesar).
-export([encode/2, decode/2,
	all_variants/1]).


encode(S, N) -> 
	encode(S, N, []).

%% +Diff fixes negative values
encode([H|T], N, Acc) 
	when H>=$a andalso H=<$z ->
	Diff = $z-$a+1,
	NewH = ((H - $a + N + Diff) rem Diff) + $a,
	encode(T, N, [NewH|Acc]);
encode([H|T], N, Acc) 
	when H>=$A andalso H=<$Z ->
	Diff = $Z-$A+1,
	NewH = ((H - $A + N + Diff) rem Diff) + $A,
	encode(T, N, [NewH|Acc]);
encode([H|T], N, Acc) ->
	encode(T, N, [H|Acc]);
encode([], _N, Acc) ->
	lists:reverse(Acc).

decode(S, N) -> 
	encode(S, -N).

all_variants(S) ->
    lists:map(fun(N) ->
            decode(S, N)
        end, lists:seq(0,25)).
