%%% -*- Mode: Prolog; Module: user; -*-

:- module(grammar,
          [ body/3,
            ast_to_xml/2
          ]).

/********************************************************************/
/*                             Grammar                              */
/********************************************************************/

body([X|Xs]) --> p(X), body(Xs).
body([]) --> [eof] | [].


p(p(P)) --> p([], P).
p(Accm, P) -->
        [word(W), spaces(_)],
        {
         append(W, " ", W1),
         append(Accm, W1, Accm1)
        },
        p(Accm1, P).
p(Accm, P) -->
        [word(W), nl(NL)],
        {
         NL >= 2,
         append(Accm, W, P)
        }.
p(Accm, P) -->
        [word(W), nl(_), eof],
        {
         append(Accm, W, P)
        }.


/********************************************************************/
/*                         XML Transformer                          */
/********************************************************************/

ast_to_xml([], "<body/>").
ast_to_xml(AST, XML) :-
        ast_to_xml(AST, "<body>", XML).



ast_to_xml([], Accm, XML) :-
        append(Accm, "</body>", XML).
ast_to_xml([p(X)|Xs], Accm, XML) :-
        append(Accm, "<p>", Open),
        append(Open, X, Content),
        append(Content, "</p>", Closed),
        ast_to_xml(Xs, Closed, XML).
        
        
