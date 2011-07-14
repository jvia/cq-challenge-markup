%%% -*- Mode: Prolog; Module: user; -*-

:- module(grammar,
          [ body/3,
            ast_to_xml/2,
            header/3
          ]).

/********************************************************************/
/*                             Grammar                              */
/********************************************************************/

body([X|Xs]) --> p(X), body(Xs).
body([X|Xs]) --> header(X), body(Xs).
body([]) --> [eof] | [].


%% Paragraphs: sloppy for now
p(p(P)) --> p([], P).
p(Accm, P) -->
        [word(W), spaces(_)],
        { append(W, " ", W1),
          append(Accm, W1, Accm1)},
        p(Accm1, P).
p(Accm, P) -->
        [word(W), nl(NL)],
        { NL >= 2,
          append(Accm, W, P)}.
p(Accm, P) -->
        [word(W), nl(_), eof],
        {append(Accm, W, P)}.
p(Accm, P) -->
        [word(W), nl(_)],
        { append(W, " ", W1),
          append(Accm, W1, Accm1)},
        p(Accm1, P).


header(h(H, Text)) --> [asterisk(H)], p(p(Text)).

/********************************************************************/
/*                         XML Transformer                          */
/********************************************************************/

ast_to_xml([], "<body/>").
ast_to_xml(AST, XML) :-
        ast_to_xml(AST, "<body>", XML).



ast_to_xml([], Accm, XML) :-
        append(Accm, "</body>", XML).
ast_to_xml([p(X)|Xs], Accm, XML) :-
        wrap("<p>", X, "</p>", Paragraph),
        append(Accm, Paragraph, Accm1),
        ast_to_xml(Xs, Accm1, XML).

wrap(Open, Content, Close, Form) :-
        append(Open, Content, Opended),
        append(Opended, Close, Form).
