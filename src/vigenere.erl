-module(vigenere).

-define(IS_ULETTER(X), ((X >= $A) andalso (X =< $Z))).
-define(IS_LLETTER(X), ((X >= $a) andalso (X =< $z))).
-define(COUNT, ($Z - $A + 1)).

% FIXME:
-compile([export_all]).

square(X, Y) when ?IS_ULETTER(X), ?IS_ULETTER(Y) ->
    ((X + Y - (2*$A)) rem ?COUNT) + $A.

resquare(X, Y) when ?IS_ULETTER(X), ?IS_ULETTER(Y) ->
    ((X - Y  + (?COUNT * 2)) rem ?COUNT) + $A.

crypt(Str, Key) ->
    Str2 = do_preprocess(Str, []),
    Key2 = do_preprocess(Key, []),
    do_crypt(fun square/2, Str2, Key2, Key2, []).

decrypt(Str, Key) ->
    Str2 = do_preprocess(Str, []),
    Key2 = do_preprocess(Key, []),
    do_crypt(fun resquare/2, Str2, Key2, Key2, []).
    
%% @doc This version of the function is without predprocessing.
crypt2(Str, Key) ->
    do_crypt(fun square/2, Str, Key, Key, []).

decrypt2(Str, Key) ->
    do_crypt(fun resquare/2, Str, Key, Key, []).

do_preprocess([H|T], Acc) 
    when ?IS_ULETTER(H) ->
    do_preprocess(T, [H|Acc]); 
do_preprocess([H|T], Acc) 
    when ?IS_LLETTER(H) ->
    NewH = $A - $a + H,
    do_preprocess(T, [NewH|Acc]); 
do_preprocess([], Acc) ->
    lists:reverse(Acc).


do_crypt(F, [SH|ST], [KH|KT], Key, Acc)
    when ?IS_ULETTER(SH) ->
    do_crypt(F, ST, KT, Key, [F(SH, KH)|Acc]);
% Skip if the letter is not in the upper case
do_crypt(F, [SH|ST], [_|KT], Key, Acc) ->
    do_crypt(F, ST, KT, Key, [SH|Acc]);
do_crypt(F, [_|_]=Str, [], Key, Acc) ->
    do_crypt(F, Str, Key, Key, Acc);
do_crypt(_, [], _, _, Acc) ->
    lists:reverse(Acc).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

square_test_() ->
    F = fun square/2,
    [?_assertEqual(F($A, $A), $A)
    ,?_assertEqual(F($A, $B), $B)
    ,?_assertEqual(F($Z, $Z), $Y)
    ,?_assertEqual(F($Z, $X), $W)
    ].

crypt_test_() ->
    F = fun crypt/2,
    [?_assertEqual(F("ATTACKATDAWN", "LEMON"), "LXFOPVEFRNHR")
    ].

crypt2_test_() ->
    F = fun crypt2/2,
    [?_assertEqual(F("CRYPTO IS SHORT FOR CRYPTOGRAPHY", 
                     "ABCDAB CD ABCDA BCD ABCDABCDABCD"), 
                     "CSASTP KV SIQUT GQU CSASTPIUAQJB")
    ].

decrypt_test_() ->
    [?_assertEqual(decrypt(crypt("ATTACKATDAWN", "LEMON"), "LEMON"), "ATTACKATDAWN")
    ].

decrypt2_test_() ->
    [?_assertEqual(decrypt2(crypt2("ATTACK AT DAWN", "LEMON"), "LEMON"), "ATTACK AT DAWN")
    ].
-endif.
