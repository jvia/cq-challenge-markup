/** <module> Markup Grammar

This module defines the grammar for the structured text known as
Markup.

@author Jeremiah Via <jxv911@cs.bham.ac.uk>
*/

%% Body grammar
body --> heading.
body --> paragraph.
body --> [].


%% Heading grammar
heading --> "*", text, nl.


%% Paragraph
paragraph --> text, nl, nl.


%% Block of text
text --> char, text.
text --> [].


%% Newline
nl --> "\n".


%% Character
char --> [C],
        {
         %% Lower case
         member(C, "abcdefghijklmnopqrstuvwxyz");
         %% Upper case
         member(C, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
         %% White space
         member(C, " _");
         %% Numbers
         member(C, "1234567890");
         %% Symbols
         member(C, "!£$%^&*(),.;'`¬\"")
        }.
