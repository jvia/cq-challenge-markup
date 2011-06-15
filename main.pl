%%% -*- Mode: Prolog; Module: main; -*-
/** <main> Runs the parser.


@author Jeremiah M. Via <jxv911@cs.bham.ac.uk>
*/
:- module(main,
          [ run/1,              % +File
            read_file/2         % +File, -Chars
          ]).


:- use_module(lexer).
:- use_module(grammar).


%%%   run(+File:atom)
run(File) :-
        read_file(File, Chars),
        %%lexer:token(Tokens, Chars, Remaining),
        token(Tokens, Chars, Remaining),
        print_tokens(Tokens),
        nl,
        print_remaining(Remaining).


%%%   read_file(+File: list, -Chars:list) is det.
%%
%%    Reads a file and returns its contents as a list of character
%%    codes.
read_file(File, Chars) :-
        see(File),
        ( read_file1([], Chars), seen )
        ;
        write('Failing to read'), nl,
        seen.


%%%   read_file1(+Accm:list, -Chars:list) is det.
%%
%%    Adds each code in the file to the list of characters.
read_file1(Accm, Chars) :-
        get_code(Code),
        (  Code == -1 ->
            append(Accm, [Code], Chars)
        ;
            append(Accm, [Code], Accm1),
            read_file1(Accm1, Chars)
        ).


%%%   print_tokens(+Tokens:list) is det.
%%
%%    Prints all of the tokens the lexer found.
print_tokens([]).
print_tokens(Tokens) :-
        write('Tokens:'), nl,
        print_tokens1(Tokens).


print_tokens1([]).
print_tokens1([Token|Tokens]) :-
        write('> '), write(Token), nl,
        print_tokens1(Tokens).


%%%   print_remaining(+Tokens:list) is det.
%%
%%    Prints the un-lexed characters.
print_remaining([]).
print_remaining(Remaining) :-
        write('Remaining Characters:'), nl,
        write(Remaining),
        nl.
