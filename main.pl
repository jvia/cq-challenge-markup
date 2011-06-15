%%% -*- Mode: Prolog; Module: main; -*-
:- [lexer, grammar].

read_file(File, Chars) :-
        see(File),
        ( read_file1([], Chars), seen )
        ;
        write('Failing to read'), nl,
        seen.


read_file1(Accm, Chars) :-
        get_code(Code),
        (  Code == -1 ->
            append(Accm, [Code], Chars)
        ;
            append(Accm, [Code], Accm1),
            read_file1(Accm1, Chars)
        ).



run(File) :-
        read_file(File, Chars),
        %%lexer:token(Tokens, Chars, Remaining),
        token(Tokens, Chars, Remaining),
        print_tokens(Tokens),
        nl,
        print_remaining(Remaining).



print_tokens([]).
print_tokens(Tokens) :-
        write('Tokens:'), nl,
        print_tokens1(Tokens).


print_tokens1([]).
print_tokens1([Token|Tokens]) :-
        write('> '), write(Token), nl,
        print_tokens1(Tokens).



print_remaining([]).
print_remaining(Remaining) :-
        write('Remaining Characters:'), nl,
        write(Remaining),
        nl.
