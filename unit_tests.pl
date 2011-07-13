%%% -*- Mode: Prolog; Module: lexer_tests; -*-
%%
%% Converts input stream into tokens.

:- use_module(library(plunit)).
:- use_module(lexer).


/********************************************************************/
/*                             Lexer Tests                          */
/********************************************************************/
:- begin_tests(lexer).

test('01_empty.txt',
     [ setup(lexer_load('01_empty', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('02_simple_paragraph',
     [ setup(lexer_load('02_simple_paragraph', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('03_multiline_paragraph',
     [ setup(lexer_load('03_multiline_paragraph', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('04_two_paragraphs',
     [ setup(lexer_load('04_two_paragraphs', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('05_several_multiline_paragraphs',
     [ setup(lexer_load('05_several_multiline_paragraphs', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('06_header',
     [ setup(lexer_load('06_header', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('07_headers',
     [ setup(lexer_load('07_headers', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('08_crazy_header',
     [setup(lexer_load('08_crazy_header', Text, FileTokens)),
      true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('09_headers_and_paragraphs',
     [setup(lexer_load('09_headers_and_paragraphs', Text, FileTokens)),
      true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('10_blockquote',
     [ setup(lexer_load('10_blockquote', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('11_multiline_blockquote',
     [ setup(lexer_load('11_multiline_blockquote', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('12_multi_paragraph_blockquote',
     [ setup(lexer_load('12_multi_paragraph_blockquote', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('13_paragraphs_and_blockquotes',
     [ setup(lexer_load('13_paragraphs_and_blockquotes', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('14_simple_verbatim',
     [ setup(lexer_load('14_simple_verbatim', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('15_useful_verbatim',
     [ setup(lexer_load('15_useful_verbatim', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('16_verbatim_with_indentation',
     [ setup(lexer_load('16_verbatim_with_indentation', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('17_verbatim_first_line_extra_indented',
     [ setup(lexer_load('17_verbatim_first_line_extra_indented', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('18_verbatim_special_xml_chars',
     [ setup(lexer_load('18_verbatim_special_xml_chars', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('19_numbered_list',
     [ setup(lexer_load('19_numbered_list', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('20_bulleted_list',
     [ setup(lexer_load('20_bulleted_list', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('21_multiparagraph_list_items',
     [ setup(lexer_load('21_multiparagraph_list_items', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('22_nested_lists',
     [ setup(lexer_load('22_nested_lists', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('23_tagged_markup',
     [ setup(lexer_load('23_tagged_markup', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('24_note_subdocument.txt',
     [ setup(lexer_load('24_note_subdocument', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('25_multiparagraph_note',
     [ setup(lexer_load('25_multiparagraph_note', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.
 
test('26_note_with_blockquote',
     [ setup(lexer_load('26_note_with_blockquote', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('27_note_with_lists',
     [ setup(lexer_load('27_note_with_lists', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('28_required_escapes',
     [ setup(lexer_load('28_required_escapes', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('29_optional_escapes',
     [ setup(lexer_load('29_optional_escapes', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('30_escaped_header',
     [ setup(lexer_load('30_escaped_header', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('31_escaped_numbered_list_marker',
     [ setup(lexer_load('31_escaped_numbered_list_marker', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('32_escaped_bullet_list_marker',
     [ setup(lexer_load('32_escaped_bullet_list_marker', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('33_escapes_not_needed',
     [ setup(lexer_load('33_escapes_not_needed', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('34_modeline',
     [ setup(lexer_load('34_modeline', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('35_links',
     [ setup(lexer_load('35_links', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('instructions',
     [ setup(lexer_load('instructions', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

test('markup-spec',
     [ setup(lexer_load('markup-spec', Text, FileTokens)),
       true(Tokens == FileTokens)
     ]) :-
        phrase(token(Tokens), Text, []), !.

:- end_tests(lexer).


/********************************************************************/
/*                            Grammar Tests                         */
/********************************************************************/
:- begin_tests(grammar).

test('01_empty',
     [ setup(grammar_load('01_empty', Tokens, File_XML)),
       true(XML == File_XML),
       fixme('not implemented')
     ]) :-
        phrase(body(XML), Tokens, []), !.

:-end_tests(grammar).



/********************************************************************/
/*                            Helper Code                           */
/********************************************************************/



read_file(File, Text) :-
        see(File),
        read_file1([], Text),
        %% Use the disjunction to that the
        %% stream is always closed.
        seen; seen.

read_file1(Accm, Text) :-
        get_code(Char),
        ( Char == -1 ->
          append(Accm, [-1], Text)
        ;
          append(Accm, [Char], New_Accm),
          read_file1(New_Accm, Text)
        ).

read_xml(File, Text) :-
        see(File),
        read_xml1([], Text),
        %% Use the disjunction to that the
        %% stream is always closed.
        seen; seen.

read_xml1(Accm, Text) :-
        get_code(Char),
        ( Char == -1 ->
          Accm = Text
        ;
          append(Accm, [Char], New_Accm),
          read_xml1(New_Accm, Text)
        ).

lexer_load(Case, Text, FileTokens) :-
        case(Case, Name),
        append("tests/", Name, Path),
        %% Load text
        append(Path, ".txt", TextPathString),
        string_to_atom(TextPathString, TextPath),
        read_file(TextPath, Text),
        %% Load lexer tokens
        append(Path, ".tokens", TokenPathString),
        string_to_atom(TokenPathString, TokenPath),
        read_list(TokenPath, FileTokens).

grammar_load(Case, Tokens, XML) :-
        case(Case, Name),
        append("tests/", Name, Path),
        %% Load tokens
        append(Path, ".tokens", TokenPathString),
        string_to_atom(TokenPathString, TokenPath),
        read_list(TokenPath, Tokens),
        %% Load XML
        append(Path, ".xml", XmlPathString),
        string_to_atom(XmlPathString, XmlPath),
        read_xml(XmlPath, XML).
        

read_list(File, List) :-
        see(File),
        read(List),
        seen ; seen.


case('01_empty',                              "01_empty").
case('02_simple_paragraph',                   "02_simple_paragraph").
case('03_multiline_paragraph',                "03_multiline_paragraph").
case('04_two_paragraphs',                     "04_two_paragraphs").
case('05_several_multiline_paragraphs',       "05_several_multiline_paragraphs").
case('06_header',                             "06_header").
case('07_headers',                            "07_headers").
case('08_crazy_header',                       "08_crazy_header").
case('09_headers_and_paragraphs',             "09_headers_and_paragraphs").
case('10_blockquote',                         "10_blockquote").
case('11_multiline_blockquote',               "11_multiline_blockquote").
case('12_multi_paragraph_blockquote',         "12_multi_paragraph_blockquote").
case('13_paragraphs_and_blockquotes',         "13_paragraphs_and_blockquotes").
case('14_simple_verbatim',                    "14_simple_verbatim").
case('15_useful_verbatim',                    "15_useful_verbatim").
case('16_verbatim_with_indentation',          "16_verbatim_with_indentation").
case('17_verbatim_first_line_extra_indented', "17_verbatim_first_line_extra_indented").
case('18_verbatim_special_xml_chars',         "18_verbatim_special_xml_chars").
case('19_numbered_list',                      "19_numbered_list").
case('20_bulleted_list',                      "20_bulleted_list").
case('21_multiparagraph_list_items',          "21_multiparagraph_list_items").
case('22_nested_lists',                       "22_nested_lists").
case('23_tagged_markup',                      "23_tagged_markup").
case('24_note_subdocument',                   "24_note_subdocument").
case('25_multiparagraph_note',                "25_multiparagraph_note").
case('26_note_with_blockquote',               "26_note_with_blockquote").
case('27_note_with_lists',                    "27_note_with_lists").
case('28_required_escapes',                   "28_required_escapes").
case('29_optional_escapes',                   "29_optional_escapes").
case('30_escaped_header',                     "30_escaped_header").
case('31_escaped_numbered_list_marker',       "31_escaped_numbered_list_marker").
case('32_escaped_bullet_list_marker',         "32_escaped_bullet_list_marker").
case('33_escapes_not_needed',                 "33_escapes_not_needed").
case('34_modeline',                           "34_modeline").
case('35_links',                              "35_links").
case('instructions',                          "instructions").
case('markup-spec',                           "markup-spec").
