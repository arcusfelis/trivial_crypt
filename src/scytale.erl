-module(scytale).
-export([crypt/2, decrypt/2, hack/3, hack/1]).

hack(Str) ->
    hack(Str, 3, string:len(Str)).

hack(Str, From, To) 
    when From<To ->
    io:format("~4B ~ts~n", [From, decrypt(Str, From)]),
    hack(Str, From+1, To);
hack(Str, To, To) ->
    ok.

crypt(Str, Key) 
    when Key>0 ->
    L = string:len(Str),
    MaxInLine = ceiling(L / Key),
    SplitedList = do_split(Str, [], MaxInLine),
    do_process(SplitedList, [], []).

decrypt(Str, Key) 
    when Key>0 ->
    SplitedList = do_split(fix_string(Str, Key), [], Key),
    do_process(SplitedList, [], []).

fix_string(Str, Key) ->
    L = string:len(Str),
    BadStrCnt = L rem Key,
    SStr = lists:reverse(Str),
    do_fix(SStr, [], BadStrCnt, Key, Key).

do_fix(T, Acc, Cnt, Rem, Rem)
    when Cnt>0, Rem>0 ->
    do_fix(T, [$ |Acc], Cnt, Rem-1, Rem);
do_fix([H|T], Acc, Cnt, Rem, OldRem)
    when Cnt>0, Rem>0 ->
    do_fix(T, [H|Acc], Cnt, Rem-1, OldRem);
do_fix(T, Acc, Cnt, 0, OldRem)
    when Cnt>0 ->
    do_fix(T, Acc, Cnt-1, OldRem, OldRem);
do_fix(T, Acc, 0, _, _OldRem)
    ->
    lists:reverse(T, Acc).
    

do_split([_|_]=Str, Acc, Max) ->
    case (catch lists:split(Max, Str)) of
    {'EXIT', _} ->
        do_split([], [Str|Acc], Max);
    {H, T} ->
        do_split(T, [H|Acc], Max)
    end;
do_split([]=_Str, Acc, _Max) ->
    lists:reverse(Acc).

do_process([[H]|TT], HAcc, TAcc) ->
    do_process(TT, [H|HAcc], TAcc); 
do_process([[H|T]|TT], HAcc, TAcc) ->
    do_process(TT, [H|HAcc], [T|TAcc]);
do_process([], HAcc, [_|_]=TAcc) ->
    do_process(lists:reverse(TAcc), HAcc, []);
do_process([], HAcc, []=_TAcc) ->
    lists:reverse(HAcc).
    
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

crypt_test_() ->
    F = fun crypt/2,
    [?_assertEqual(F("HELPMEIAMUNDERATTACK", 4), "HENTEIDTLAEAPMRCMUAK")
    ].

decrypt_test_() ->
    D = fun decrypt/2,
    C = fun crypt/2,
    [?_assertEqual(D(C("HELPMEIAMUNDERATTACK", 4), 4), "HELPMEIAMUNDERATTACK")
    ,?_assertEqual(D(C("HELPMEIAMUNDERATTA", 4), 4), "HELPMEIAMUNDERATTA  ")
    ].


-endif.
