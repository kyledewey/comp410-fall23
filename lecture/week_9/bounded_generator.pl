decBound(In, Out) :-
    In > 0,
    Out is In - 1.

boundedExpression(_, true).
boundedExpression(_, false).
boundedExpression(B1, and(E1, E2)) :-
    decBound(B1, B2),
    boundedExpression(B2, E1),
    boundedExpression(B2, E2).
boundedExpression(B1, or(E1, E2)) :-
    decBound(B1, B2),
    boundedExpression(B2, E1),
    boundedExpression(B2, E2).
boundedExpression(B1, not(E)) :-
    decBound(B1, B2),
    boundedExpression(B2, E).
