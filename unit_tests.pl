:- use_module(library(plunit)).
:- use_module(lexer).

%% This will be moved to a different file once the grammar is
%% finished.
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


read_list(File, List) :-
        see(File),
        read(List),
        seen ; seen.

:- begin_tests(lexer).


test('01_empty.txt', [nondet]) :-
        read_file('tests/01_empty.txt',    Text),
        read_list('tests/01_empty.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('02_simple_paragraph.txt', [nondet]) :-
        read_file('tests/02_simple_paragraph.txt',    Text),
        read_list('tests/02_simple_paragraph.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('03_multiline_paragraph.txt', [nondet]) :-
        read_file('tests/03_multiline_paragraph.txt',    Text),
        read_file('tests/03_multiline_paragraph.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('04_two_paragraphs.txt', [nondet]) :-
        read_file('tests/04_two_paragraphs.txt', Text),
        read_list('tests/04_two_paragraphs.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('05_several_multiline_paragraphs.txt', [nondet]) :-
        read_file('tests/05_several_multiline_paragraphs.txt', Text),
        read_list('tests/05_several_multiline_paragraphs.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('06_header.txt', [nondet]) :-
        read_file('tests/06_header.txt',    Text),
        read_list('tests/06_header.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('07_headers.txt', [nondet]) :-
        read_file('tests/07_headers.txt',    Text),
        read_list('tests/07_headers.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('08_crazy_header.txt', [nondet]) :-
        read_file('tests/08_crazy_header.txt',    Text),
        read_list('tests/08_crazy_header.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('09_headers_and_paragraphs.txt', [nondet]) :-
         read_file('tests/09_headers_and_paragraphs.txt',   Text),
         read_list('tests/09_headers_and_paragraphs.tokens', Tokens),
         phrase(token(Tokens), Text, []).

test('10_blockquote.txt', [nondet]) :-
        read_file('tests/10_blockquote.txt',   Text),
        read_list('tests/10_blockquote.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('11_multiline_blockquote.txt', [nondet]) :-
        read_file('tests/11_multiline_blockquote.txt',   Text),
        read_list('tests/11_multiline_blockquote.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('12_multi_paragraph_blockquote.txt', [nondet]) :-
        read_file('tests/12_multi_paragraph_blockquote.txt',   Text),
        read_list('tests/12_multi_paragraph_blockquote.tokens', Tokens),
        phrase(token(Tokens), Text, []).

test('13_paragraphs_and_blockquotes.txt', [nondet]) :-
        read_file('tests/13_paragraphs_and_blockquotes.txt',   Text),
        read_list('tests/13_paragraphs_and_blockquotes.tokens', Tokens),
        phrase(token(Tokens), Text, []).
/*    
test('14_simple_verbatim.txt', [nondet]) :-
        read_file('tests/14_simple_verbatim.txt',   Text),
        read_list('tests/14_simple_verbatim.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('15_useful_verbatim.txt', [nondet]) :-
        read_file('tests/15_useful_verbatim.txt',   Text),
        read_list('tests/15_useful_verbatim.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('16_verbatim_with_indentation.txt', [nondet]) :-
        read_file('tests/16_verbatim_with_indentation.txt',   Text),
        read_list('tests/16_verbatim_with_indentation.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('17_verbatim_first_line_extra_indented.txt', [nondet]) :-
        read_file('tests/17_verbatim_first_line_extra_indented.txt',   Text),
        read_list('tests/17_verbatim_first_line_extra_indented.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('18_verbatim_special_xml_chars.txt', [nondet]) :-
        read_file('tests/18_verbatim_special_xml_chars.txt',   Text),
        read_list('tests/18_verbatim_special_xml_chars.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('19_numbered_list.txt', [nondet]) :-
        read_file('tests/19_numbered_list.txt',   Text),
        read_list('tests/19_numbered_list.tokens', Tokens),
        phrase(token(Tokens), Text, []).
  
test('20_bulleted_list.txt', [nondet]) :-
        read_file('tests/20_bulleted_list.txt',   Text),
        read_list('tests/20_bulleted_list.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('21_multiparagraph_list_items.txt', [nondet]) :-
        read_file('tests/21_multiparagraph_list_items.txt',   Text),
        read_list('tests/21_multiparagraph_list_items.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('22_nested_lists.txt', [nondet]) :-
        read_file('tests/22_nested_lists.txt',   Text),
        read_list('tests/22_nested_lists.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('23_tagged_markup.txt', [nondet]) :-
        read_file('tests/23_tagged_markup.txt',   Text),
        read_list('tests/23_tagged_markup.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('24_note_subdocument.txt', [nondet]) :-
        read_file('tests/24_note_subdocument.txt',   Text),
        read_list('tests/24_note_subdocument.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('25_multiparagraph_note.txt', [nondet]) :-
        read_file('tests/25_multiparagraph_note.txt',   Text),
        read_list('tests/25_multiparagraph_note.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('26_note_with_blockquote.txt', [nondet]) :-
        read_file('tests/26_note_with_blockquote.txt',   Text),
        read_list('tests/26_note_with_blockquote.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('27_note_with_lists.txt', [nondet]) :-
        read_file('tests/27_note_with_lists.txt',   Text),
        read_list('tests/27_note_with_lists.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('28_required_escapes.txt', [nondet]) :-
        read_file('tests/28_required_escapes.txt',   Text),
        read_list('tests/28_required_escapes.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('29_optional_escapes.txt', [nondet]) :-
        read_file('tests/29_optional_escapes.txt',   Text),
        read_list('tests/29_optional_escapes.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('30_escaped_header.txt', [nondet]) :-
        read_file('tests/30_escaped_header.txt',   Text),
        read_list('tests/30_escaped_header.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('31_escaped_numbered_list_marker.txt', [nondet]) :-
        read_file('tests/31_escaped_numbered_list_marker.txt',   Text),
        read_list('tests/31_escaped_numbered_list_marker.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('32_escaped_bullet_list_marker.txt', [nondet]) :-
        read_file('tests/32_escaped_bullet_list_marker.txt',   Text),
        read_list('tests/32_escaped_bullet_list_marker.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('33_escapes_not_needed.txt', [nondet]) :-
        read_file('tests/33_escapes_not_needed.txt',   Text),
        read_list('tests/33_escapes_not_needed.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('34_modeline.txt', [nondet]) :-
        read_file('tests/34_modeline.txt',   Text),
        read_list('tests/34_modeline.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('35_links.txt', [nondet]) :-
        read_file('tests/35_links.txt',   Text),
        read_list('tests/35_links.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('instructions.txt', [nondet]) :-
        read_file('tests/instructions.txt',   Text),
        read_list('tests/instructions.tokens', Tokens),
        phrase(token(Tokens), Text, []).
test('markup-spec.txt', [nondet]) :-
        read_file('tests/markup-spec.txt',   Text),
        read_list('tests/markup-spec.tokens', Tokens),
        phrase(token(Tokens), Text, []).
*/
:- end_tests(lexer).
