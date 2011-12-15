-module(scytale).
-export([hack/1, crypt/2, decrypt/2, echo/2]).
-compile([export_all]).

hack(Str) ->
    hack(Str, 2, string:len(Str)).

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

echo(Str, Key) ->
    L = string:len(Str),
    MaxInLine = ceiling(L / Key),
    SplitedList = do_split(Str, [], MaxInLine),
    do_echo(SplitedList, Key).

do_echo([H|T], Key) ->
    io:format("~ts~n", [H]),
    do_echo(T, Key-1);
do_echo([], Key)
    when Key>0 ->
    io:format("???~n"),
    do_echo([], Key-1);
do_echo([], 0) -> ok.
    
    
% Key = 4
%
% x x x 
% s s s
% d d d
% _ _ _

% Key = 6
%
% XX
% XS
% SS
% DD
% D_
% __


%% Add whitespaces to the end of the SOURCE string.
%% Str is an encrypted string.
fix_string(Str, Key) ->
    L = string:len(Str),
    case L rem Key of
    0 ->
        Str;
    _ ->
        Width = (L div Key) + 1,
        SStr = lists:reverse(Str),
        FullLen = Width * Key,
        Remains = FullLen - L,
        SuffixAdd = Remains rem Width,
        Add = 
            case SuffixAdd of
            0 -> 0;
            _ -> 1
            end + Remains div Width,

        io:format("Remains=~B Add=~B SAdd=~B Key=~B ~n", [Remains, Add, SuffixAdd, Key]),
        do_fix(SStr, [], Remains, SuffixAdd, Add, Key, Key)
    end.

add(C, X, Acc)
    when C>0 ->
    add(C-1, X, [X|Acc]);
add(_, _, Acc) ->
    Acc.

do_fix(T, Acc, Cnt, Suf=1, Add, Rem, Rem)
    when Cnt>0, Rem>0, Add>0 ->
    do_fix(T, add(Add, $ , Acc), Cnt-Add, Suf-1, Add-1, Rem-Add, Rem);
do_fix(T, Acc, Cnt, Suf, Add, Rem, Rem)
    when Cnt>0, Rem>0, Add>0 ->
    do_fix(T, add(Add, $ , Acc), Cnt-Add, Suf-1, Add, Rem-Add, Rem);
do_fix([H|T], Acc, Cnt, Suf, Add, Rem, OldRem)
    when Cnt>0, Rem>0 ->
    do_fix(T, [H|Acc], Cnt, Suf, Add, Rem-1, OldRem);
do_fix(T, Acc, Cnt, Suf, Add, 0, OldRem)
    when Cnt>0 ->
    do_fix(T, Acc, Cnt, Suf, Add, OldRem, OldRem);
do_fix(T, Acc, 0, _Suf, _Add, _Rem, _OldRem) ->
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

fix_string_test_() ->
    F = fun fix_string/2,
    [?_assertEqual(F("XXSDXSSD", 6),  "XXSD  XSSD  ")
    ,?_assertEqual(F("XXSDDXSSD", 6), "XXSDD XSSD  ")
    ,?_assertEqual(F("adgjmpsvybehknqtwzcfilorux", 11), "adgjmpsvy  behknqtwz  cfilorux   ")
    ].

-endif.
