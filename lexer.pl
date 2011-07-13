%%% -*- Mode: Prolog; Module: lexer; -*-
%%
%% Converts input stream into tokens.

:- module(lexer,
          [ token/3,            % CodeList
            bracket/3
          ]).


/********************************************************************/
/*                            Lexer Grammar                         */
/********************************************************************/

token(Xs)     --> modeline,  token(Xs).
token([X|Xs]) --> markup(X), token(Xs).
token([X|Xs]) --> escape(X), token(Xs).
token([X|Xs]) --> lookahead("*"),  asterisk(0, X), token(Xs).
token([X|Xs]) --> lookahead(" "),  spaces(0, X), token(Xs).
token([X|Xs]) --> lookahead("\n"), nl(0, X), token(Xs).
token([X|Xs]) --> bracket(X),  token(Xs).
token([X|Xs]) --> lookahead([C]), {is_plain_char(C)}, word(X), token(Xs).
token([X|Xs]) --> hash(X), token(Xs).
token([X|Xs]) --> dash(X), token(Xs).
token([X|Xs]) --> pipe(X), token(Xs).
token([eof])  --> end_of_file.
token([neof]) --> [].

%% Note of a hash mark because it may become an ordered list item.
hash(hash) --> "#".


%% Note of a dash mark because it may become an ordered list item.
dash(dash) --> "-".


%% Catch pipes because they have syntactic importance
pipe(pipe) --> "|".


%% Catch everything that may be a potentially escaped character.
escape(esc(C)) --> "\\", [C].


%% Count the number of consecutive spaces and group together for
%% easier processing in the grammar.
spaces(X, Y)  --> " ", {X1 is X + 1}, spaces(X1, Y).
spaces(X, spaces(X)) --> [].


%% Count the number of consecutive newlines and group together for
%% easier processing in the grammar.
nl(X, Y) --> "\n", {X1 is X + 1}, nl(X1, Y).
nl(X, nl(X)) --> [].


%% Count the number of consecutive asterisks and group together for
%% easier processing in the grammar.
asterisk(X, Y) --> "*", {X1 is X + 1}, asterisk(X1, Y).
asterisk(X, asterisk(X)) --> [].


%% Group together characters into words. This includes punctuation
%% because they make no difference in this task.
word(word(X)) --> word1(X).
word1([X|Xs]) --> char(X),  word1(Xs).
word1([])     --> lookahead(C), {is_word_end(C)} | [].


%% Tokenize a character which has no syntactic potential.
char(C) --> [C], {is_plain_char(C)}.


%% Make a note of brackets since they tend to have syntactic
%% importance.
bracket(open(Bracket))  --> [Bracket], {member(Bracket, "([{<")}.
bracket(close(Bracket)) --> [Bracket], {member(Bracket, ")]}>")}.


%% Catch markup here to make easier processing in the grammar.
markup(italic) --> "\\i{".
markup(bold)   --> "\\b{".
markup(code)   --> "\\code{".
markup(note)   --> "\\note{".
markup(url)    --> "\\url{".


%% Catch the modeline so we can ignore it.
modeline --> "-*- mode: markup; -*-".


%% End of file character
end_of_file --> [-1].


/********************************************************************/
/*                            Helper Code                           */
/********************************************************************/

%% Peek one character ahead
lookahead([C]), [C] --> [C].


is_plain_char(C) :-
        "!" =< C,
        C =\= -1,
        %% Characters of syntactic importance.
        \+ member(C, "([{<|>})]#-*\\").


is_word_end(C) :-
        name(' ', [C]) ;
        name('\n', [C]) ;
        -1 == C.
