booleanExpression(true).
booleanExpression(false).
booleanExpression(and(E1, E2)) :-
    booleanExpression(E1),
    booleanExpression(E2).
booleanExpression(or(E1, E2)) :-
    booleanExpression(E1),
    booleanExpression(E2).
booleanExpression(not(E)) :-
    booleanExpression(E).
