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

:- module(grammar,
          [ eof/1               % enf_of_file
          ]).
:- dynamic indent/1.

indent(0).


/********************************************************************/
/*                              Grammar                             */
/********************************************************************/


/********************************************************************/
/*                              Lexicon                             */
/********************************************************************/


eof --> [-1].
