%%% -*- Mode: Prolog; Module: lexer; -*-
%%
%% Converts input stream into tokens.

%% Lex markup environments
token([X|Xs]) -->
        markup(X),
        token(Xs).
%% Lex escape
token([X|Xs]) -->
        escape(X),
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
        { %% Special markup
          special(Special),
          \+ member(C, Special)
        },
        word(X),
        token(Xs).
%% Lex hash
token([hash|Xs]) -->
        hash,
        token(Xs).
%% Lex dash
token([dash|Xs]) -->
        dash,
        token(Xs).
%% Empty or eof
token([]) --> [].


hash --> "#".


dash --> "-".


escape(esc(C)) --> "\\", [C].

%% Potentially too permissive
%%markup(markup(E)) --> "\\", [E], "{".


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
         %% Special markup
         special(Special),
         \+ member(C, Special)
        }.



%% Markup
markup(M) -->  i(M) | b(M) | code(M) | note(M).
        
i(i_) --> "\\i{".


b(b_) --> "\\b{".


code(code_) --> "\\code{".


note(note_) --> "\\note{".



%% Peek one character ahead
lookahead(C), [C] --> [C].

%% List of characters of potential syntactic importance
special("}#-").