%%% The realization of [ROT13](http://en.wikipedia.org/wiki/ROT13)

-module(rot13).
-export([encode/1]).


encode(S) -> 
	encode(S, []).

%% +Diff fixes negative values
encode([H|T], Acc) ->
    NewH = enc("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
              ,"NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"
              ,H),
	encode(T, [NewH|Acc]);
encode([], Acc) ->
	lists:reverse(Acc).


enc([H1|_], [H2|_], H1) ->
    H2;
enc([_|T1], [_|T2], H) ->
    enc(T1, T2, H);
enc([], [], H) -> H.
