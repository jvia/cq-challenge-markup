/** <module> Markup Grammar

This module defines the grammar for the structured text known as
Markup.

@author Jeremiah Via <jxv911@cs.bham.ac.uk>
*/


%% This will be moved to a different file once the grammar is
%% finished.
read_file(File, Text) :-
        see(File),
        read_file1([], Text),
        %% Use the disjunction to that the
        %% stream is always closed.
        close(File), !
        ;
        close(File), !.

read_file1(Accm, Text) :-
        get_char(Char),
        ( Char == end_of_file ->
            Accm = Text
        ;
            append(Accm, [Char], New_Accm),
            read_file1(New_Accm, Text)
        ).


/********************************************************************/
/*                              Grammar                             */
/********************************************************************/


%% Handle the optinonal mode line
mode_line --> ['-*- mode: markup; -*-'], blank_line.
mode_line --> [].


%% Handles the headings
h --> ['*'], h.
h --> [].


%% Handles a paragraph
p --> text, blank_line.


%% Handles groups of text
text --> alpha, text.
text --> digit, text.
text --> whitespace, text.
text --> punct, text.
text --> [].


%% Blank line must be at least two newlines or newline and end_of_file
%% marker.
blank_line --> nl, extra_nl.


extra_nl --> nl, extra_nl.
extra_nl --> ['end_of_file'].
extra_nl --> [].


/********************************************************************/
/*                              Lexicon                             */
/********************************************************************/


nl --> ['\n'].

digit --> [C], { name(C, [V]), V >= 48, V =< 57 }.

whitespace --> [' '] | ['\t'].

punct --> ['.'] | ['?'] | ['!'], [','] | [';'].

alpha -->
        [C],
        { name(C, [V]),
          (
            97 =< V, V =< 122
          ;
            65 =< V, V =< 90
          )
        }.
