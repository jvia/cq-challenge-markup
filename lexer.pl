%%% -*- Mode: Prolog; Module: user; -*-
%%
%% Converts input stream into tokens.

%% Lex italic
token([X|Xs]) -->
        i(X),
        token(Xs).
%% Lex bold
token([X|Xs]) -->
        b(X),
        token(Xs).
%% Lex }
token([X|Xs]) -->
        endB(X),
        token(Xs).
%% Lex spaces
token([X|Xs]) -->
        lookahead(C),
        {name(' ', [C])},
        spaces(0, X),
        token(Xs).
%% Lex newline
token([X|Xs]) -->
        lookahead(C),
        {name('\n', [C])},
        nl(0, X),
        token(Xs).
%% Lex words
token([word(X)|Xs]) -->
        lookahead(C),
        {"!" =< C, C =< "~"},
        word(X),
        token(Xs).
token([]) --> [].


i(i_) --> "\\i{".


b(b_) --> "\\b{".



endB('}') --> "}".


spaces(X, Y)  --> " ", {X1 is X + 1}, spaces(X1, Y).
spaces(X, spaces(X)) --> [].


nl(X, Y) --> "\n", {X1 is X + 1}, nl(X1, Y).
nl(X, nl(X)) --> [].


word([X|Xs]) -->
        char(X),
        word(Xs).
word([]) -->
        lookahead(C),
        {name(' ', [C])}.
word([]) --> [].


char(C) -->
        [C],
        {
         "!" =< C,
         C =< "~",
         C =\= "}"
        }.
        


%% Peek one character ahead
lookahead(C), [C] --> [C].
