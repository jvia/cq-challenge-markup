%%% -*- Mode: Prolog; Module: lexer; -*-
%%
%% Converts input stream into tokens.

:- module(lexer,
          [ token/3,            % CodeList
            bracket/3
          ]).


%% Lex markup environments
token(Xs)     --> modeline, token(Xs).
token([X|Xs]) --> markup(X), token(Xs).
token([X|Xs]) --> escape(X), token(Xs).
token([X|Xs]) --> lookahead("*"), asterisk(0, X), token(Xs).
token([X|Xs]) --> lookahead(" "), spaces(0, X), token(Xs).
token([X|Xs]) --> lookahead("\n"), nl(0, X), token(Xs).
token([X|Xs]) --> bracket(X), token(Xs).
token([word(X)|Xs]) -->
        lookahead([C]),
        { is_plain_char(C) },
        word(X),
        token(Xs).
token([hash|Xs]) --> hash, token(Xs).
token([dash|Xs]) --> dash, token(Xs).
token([eof]) --> [C], {eof(C)}.
%% May be error if not from command line
token([no-eof]) --> [].


hash --> "#".
dash --> "-".


escape(esc(C)) --> "\\", char(C).


spaces(X, Y)  --> " ", {X1 is X + 1}, spaces(X1, Y).
spaces(X, spaces(X)) --> [].


nl(X, Y) --> "\n", {X1 is X + 1}, nl(X1, Y).
nl(X, nl(X)) --> [].


asterisk(X, Y) --> "*", {X1 is X + 1}, asterisk(X1, Y).
asterisk(X, asterisk(X)) --> [].


word([X|Xs]) --> char(X),  word(Xs).
word([]) --> lookahead(C), { is_word_end(C) } | [].


char(C) --> [C], { is_plain_char(C) }.


bracket(open(Bracket))  -->
        [Bracket], {member(Bracket, "([{<")}.
bracket(close(Bracket)) -->
        [Bracket], {member(Bracket, ")]}>")}.




%% Markup
markup(italic) --> "\\i",    bracket(open("{")).
markup(bold)   --> "\\b",    bracket(open("{")).
markup(code)   --> "\\code", bracket(open("{")).
markup(note)   --> "\\note", bracket(open("{")).


%% List of characters of potential syntactic importance
special("([{<)]}>#-*").

eof(-1).
modeline --> "-*- mode: markup; -*-".


/*******************************/
/*         Helper code         */
/*******************************/

%% Peek one character ahead
lookahead([C]), [C] --> [C].


is_plain_char(C) :-
        "!" =< C, C =< "~",
        C =\= -1,
        %% Special markup
        special(Special),
        \+ member(C, Special).


is_word_end(C) :-
        name(' ', [C]) ;
        name('\n', [C]) ;
        -1 == C.


        
