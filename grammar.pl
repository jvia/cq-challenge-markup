%%% -*- Mode: Prolog; Module: user; -*-
%%
%%
%% This module defines the grammar.
%%
%%
%% TODO:
%%   - [ ] Paragraph
%%      - [ ] Can contain tagged markup
%%      - [ ] Terminates with at least two newlines or
%%            possibly an end_of_file marker
%%   - [ ] Headers
%%      - [ ] Special pragraph tha begins with '*'
%%      - [ ] No limit to '*'
%%      - [ ] Should be able to keep track of the level in the
%%            hierarchy
%%      - [ ] Besides '*', they function the same as paragraphs
%%   - [ ] Block quotes
%%      - [ ] Two spaces relative to the indentation level
%%      - [ ] Ends with end_of_file or less indentation
%%      - [ ] can contain pragraphs
%%   - [ ] Verbatim section
%%      - [ ] Three spaces relative to the enclosing section
%%      - [ ] All text captured exactly as is
%%   - [ ] Lists
%%      - [ ] Two spaces of indentation followed by '-' or '#'
%%      - [ ] '#' is ordered
%%      - [ ] '-' unordered
%%      - [ ] can be nested
%%      - [ ] can contain paragraphs
%%   - [ ] Links
%%      - [ ] Unnamed linked '\url{}'
%%      - [ ] Key link '[url|key]' which links to '[key|url]' at
%%            the end of the document
%%   - [ ] Tagged Markup
%%      - [ ] italic: \i{}
%%      - [ ] bold: \b{}
%%      - [ ] code: \code{}
%%      - [ ] note: \note{}
%%      - [ ] Some markup is a subdocument rather than markup
%%   - [ ] Escapes
%%      - [ ] Backslash can escape any character at the location
%%            where it has syntactic significance
%%   - [ ] Mode line
%%
%% @author Jeremiah Via <jxv911@cs.bham.ac.uk>

:- dynamic indent/1, last_indent/1.

indent(0).
last_indent(0).


/********************************************************************/
/*                              Grammar                             */
/********************************************************************/
body --> mode, p.
body --> p.


p --> [].

text([Char|Chars]) -->
        char(Char),
        text(Chars).
text([Char|Chars]) -->
        punct(Char),
        text(Chars).
text([Char|Chars]) -->
        whitespace(Char),
        text(Chars). 
text([]) --> [].


/********************************************************************/
/*                              Lexicon                             */
/********************************************************************/

mode --> "-*- mode: markup; -*-".


nl_spaces --> " ", {
                 indent(I),
                 retract(indent(I)),
                 I1 is I + 1,
                 assert(indent(I1))
                }, spaces.
nl_spaces --> [].



punct(C) --> [C], {member(C, ".!?,;'")}.


whitespace(C) -->
        [C],
        {member(C, " \t")}.
        
%% Covers only basic text right now; 
%% special characters later
char(C) --> [C], { "!" =< C, C =< "~" }.
