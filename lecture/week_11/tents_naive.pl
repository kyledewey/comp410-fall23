% Derived from: https://www.brainbashers.com/tents.asp
% Board can contain the following:
% - tree (fixed)
% - empty
% - tent
%
% Rules for tent placement:
% - A tent must be horizontally or vertically next to a tree
% - A tent cannot be next to another tent, either horizontally,
%   vertically, or diagnally
% - The numbers in the rows and columns specify the number of
%   tents which need to be in the given row and column.
% - Tents are connected to one tree, and trees can be connected
%   to at most one tent

% Gets the square at the given row and column, in a singleton
% list.  If the row and column is out of bounds, it returns
% an empty list
%
% getSquare: Board, Row, Column, [Item]
getSquare(Board, Row, Column, Result) :-
    (nth0(Row, Board, TargetRow),
     nth0(Column, TargetRow, Item)) ->
        Result = [Item];
        Result = [].

% like getSquare, but will return a pair if the position
% holds a tree
getSquareTreePair(Board, Row, Column, Result) :-
    getSquare(Board, Row, Column, Maybe),
    (Maybe == [tree] ->
         Result = [pair(Row, Column)];
         Result = []).

% Returns the pairs for which there is a tree in this
% position
% treeSquares: Board, Row, Column, [pair(Row, Column)]
treeSquares(Board, Row, Column, Pairs) :-
    RowMinOne is Row - 1,
    RowPlusOne is Row + 1,
    ColMinOne is Column - 1,
    ColPlusOne is Column + 1,
    getSquareTreePair(Board, RowMinOne, Column, Up),
    getSquareTreePair(Board, RowPlusOne, Column, Down),
    getSquareTreePair(Board, Row, ColMinOne, Left),
    getSquareTreePair(Board, Row, ColPlusOne, Right),
    flatten([Up, Down, Left, Right], Pairs).

% tentNearby: Board, Row, Column
tentNearby(Board, Row, Column) :-
    RowMinOne is Row - 1,
    RowPlusOne is Row + 1,
    ColMinOne is Column - 1,
    ColPlusOne is Column + 1,
    (getSquare(Board, RowMinOne, ColMinOne, Target); % up left
     getSquare(Board, RowMinOne, Column, Target); % up
     getSquare(Board, RowMinOne, ColPlusOne, Target); % up right
     getSquare(Board, Row, ColMinOne, Target); % left
     getSquare(Board, Row, ColPlusOne, Target); % right
     getSquare(Board, RowPlusOne, ColMinOne, Target); % down left
     getSquare(Board, RowPlusOne, Column, Target); % down
     getSquare(Board, RowPlusOne, ColPlusOne, Target)), % down right
    Target == [tent].

tentPositionsOkNextPosition(Board, Connected, Row, Col) :-
    nth0(0, Board, FirstRow),
    length(Board, NumRows),
    length(FirstRow, NumCols),
    RowLimit is NumRows - 1,
    ColLimit is NumCols - 1,
    ((Col < ColLimit, % increment column
      NextCol is Col + 1,
      tentPositionsOk(Board, Connected, Row, NextCol));
     (Col == ColLimit, % increment row
      Row < RowLimit,
      NextRow is Row + 1,
      tentPositionsOk(Board, Connected, NextRow, 0));
     (Col == ColLimit, % complete
      Row == RowLimit)).

% Basic idea: walk over the board, and make sure that each
% tent we find is not next to another tent, and that
% it is connected to a tree.  No two tents are allowed
% to connect to the same tree
%
% tentPositionsOk: Board, Connected, Row, Col
tentPositionsOk(Board, Connected, Row, Col) :-
    % Case #1: We have a tree here.  Move on.
    getSquare(Board, Row, Col, [Tree]),
    Tree == tree,
    tentPositionsOkNextPosition(Board, Connected, Row, Col).
tentPositionsOk(Board, Connected, Row, Col) :-
    % Case #2: We have a tent here.  Make sure it's not
    % near another tent, and that it's connected to a tree
    getSquare(Board, Row, Col, [tent]),
    \+ tentNearby(Board, Row, Col),
    treeSquares(Board, Row, Col, TreePositions),
    member(ConnectHere, TreePositions),
    \+ member(ConnectHere, Connected),
    tentPositionsOkNextPosition(Board, [ConnectHere|Connected], Row, Col).
tentPositionsOk(Board, Connected, Row, Col) :-
    % Case #3: This position is empty.  Always legal.
    getSquare(Board, Row, Col, [empt]),
    tentPositionsOkNextPosition(Board, Connected, Row, Col).

% tentPositionsOk: Board
tentPositionsOk(Board) :-
    tentPositionsOk(Board, [], 0, 0).

% numTents: List, NumTents
numTents([], 0).
numTents([tent|Rest], Num) :-
    numTents(Rest, RestNum),
    Num is RestNum + 1.
numTents([H|T], Num) :-
    H \== tent,
    numTents(T, Num).

% numTentsOkInRows: Board, RowNums
numTentsOkInRows([], []).
numTentsOkInRows([Row|Rows], [Expected|Rest]) :-
    numTents(Row, Expected),
    numTentsOkInRows(Rows, Rest).

% getColumn: Board, ColumnNumber, Column
getColumn([], _, []).
getColumn([H|T], ColumnNum, [Column|Rest]) :-
    nth0(ColumnNum, H, Column),
    getColumn(T, ColumnNum, Rest).


% numTentsOkInCols: Board, ColNums, ColumnNumber
numTentsOkInCols(_, [], _).
numTentsOkInCols(Board, [Expected|Rest], ColumnNumber) :-
    getColumn(Board, ColumnNumber, Column),
    numTents(Column, Expected),
    NewColumnNumber is ColumnNumber + 1,
    numTentsOkInCols(Board, Rest, NewColumnNumber).

% numTentsOkInCols: Board, ColNums
numTentsOkInCols(Board, ColNums) :-
    numTentsOkInCols(Board, ColNums, 0).

% isSolution: Board, RowNums, ColNums
isSolution(Board, RowNums, ColNums) :-
    tentPositionsOk(Board),
    numTentsOkInRows(Board, RowNums),
    numTentsOkInCols(Board, ColNums).

test1(test(Board, RowNums, ColNums)) :-
    Board = [[_, tree, _, _, _],
             [_, _, _, _, tree],
             [_, tree, _, tree, _],
             [_, _, _, _, tree],
             [_, _, _, _, _]],
    RowNums = [2, 0, 2, 0, 1],
    ColNums = [2, 0, 1, 0, 2].

test2(test(Board, RowNums, ColNums)) :-
    Board = [[_, tree, _, _, _, _],
             [tree, _, _, _, _, _],
             [_, _, tree, _, tree, _],
             [_, _, _, _, tree, _],
             [_, _, tree, _, tree, _],
             [_, _, _, _, _, _]],
    RowNums = [2, 1, 1, 1, 1, 1],
    ColNums = [1, 2, 1, 1, 2, 0].

test3(test(Board, RowNums, ColNums)) :-
    Board = [[_, _, tree, _, _, _, _, _],
             [tree, _, _, tree, _, _, _, _],
             [_, _, _, _, _, tree, _, _],
             [_, _, _, tree, _, _, _, tree],
             [_, _, _, _, _, _, _, _],
             [_, tree, _, _, _, _, tree, _],
             [tree, tree, _, _, _, _, _, _],
             [_, _, _, _, _, tree, _, tree]],
    RowNums = [2, 1, 2, 1, 1, 1, 3, 1],
    ColNums = [3, 1, 1, 1, 1, 2, 1, 2].

test4(test(Board, RowNums, ColNums)) :-
    Board = [[_, _, _, _, tree, _, _, _],
             [tree, _, _, _, _, _, _, tree],
             [_, _, tree, _, _, _, _, _],
             [_, _, _, tree, _, _, _, _],
             [_, _, _, _, _, tree, _, _],
             [_, tree, _, _, _, _, _, tree],
             [_, _, _, _, _, _, _, _],
             [_, tree, tree, _, tree, _, tree, _]],
    RowNums = [2, 1, 2, 0, 2, 1, 2, 2],
    ColNums = [3, 1, 1, 2, 2, 0, 1, 2].

test5(test(Board, RowNums, ColNums)) :-
    Board = [[_, tree, _, tree, _, tree, _, _, _, tree, _, _],
             [tree, _, _, _, _, _, tree, _, _, _, _, tree],
             [_, _, _, _, tree, _, _, tree, _, _, _, _],
             [_, _, _, _, _, _, _, tree, _, _, _, _],
             [_, tree, _, _, tree, _, _, _, tree, _, _, _],
             [tree, _, _, _, _, _, _, _, _, _, tree, _],
             [_, _, _, _, tree, _, _, _, tree, _, _, _],
             [tree, _, _, _, _, _, tree, _, _, _, _, tree],
             [_, _, _, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, tree, _, _, _, _],
             [_, _, _, tree, _, tree, _, _, _, _, _, _],
             [_, tree, _, _, tree, _, tree, _, tree, tree, _, _]],
    RowNums = [5, 0, 3, 2, 2, 3, 1, 3, 1, 3, 1, 4],
    ColNums = [1, 4, 1, 2, 4, 2, 2, 3, 2, 2, 2, 3].

test6(test(Board, RowNums, ColNums)) :-
    Board = [[_, _, _, tree, _, _, _, tree, _, _, _, tree],
             [tree, _, _, _, tree, _, _, tree, _, _, _, _],
             [_, tree, _, _, _, _, _, _, _, _, tree, _],
             [_, _, tree, tree, _, _, _, _, _, tree, _, tree],
             [tree, _, _, _, _, tree, _, _, _, _, _, _],
             [_, tree, _, _, _, _, _, _, _, tree, _, _],
             [_, _, _, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, tree, tree, _, _, _, tree, _],
             [_, _, _, _, _, _, _, _, _, _, tree, _],
             [_, _, _, _, _, tree, _, tree, tree, _, _, _],
             [_, tree, tree, _, _, _, _, _, _, tree, _, _],
             [_, _, _, _, tree, _, _, _, _, _, tree, _]],
    RowNums = [5, 1, 4, 0, 3, 2, 2, 1, 3, 1, 2, 4],
    ColNums = [2, 3, 3, 2, 2, 2, 2, 1, 3, 2, 2, 4].

test7(test(Board, RowNums, ColNums)) :-
    Board = [[_, _, _, tree, _, tree, _, _, _, _, _, _, _, _, tree, _],
             [tree, tree, _, _, _, _, _, _, _, _, _, tree, _, tree, _, tree],
             [_, _, _, _, tree, _, _, tree, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, tree, _, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, _, tree, _, _, tree, tree, _, _, _],
             [tree, _, tree, tree, _, _, tree, _, _, _, _, _, _, _, tree, _],
             [_, _, tree, _, _, _, _, _, _, tree, _, _, _, _, tree, _],
             [tree, _, _, tree, _, _, _, tree, _, tree, _, _, _, _, _, _],
             [_, _, _, tree, tree, _, _, _, _, _, tree, tree, _, _, _, _],
             [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
             [tree, _, _, tree, _, _, _, tree, _, _, _, _, _, tree, _, tree],
             [_, _, _, _, _, _, _, _, tree, _, _, tree, _, _, _, tree],
             [_, _, _, _, _, tree, _, _, _, _, _, _, _, _, _, _],
             [_, _, tree, _, tree, _, tree, _, _, tree, _, _, _, tree, _, tree],
             [tree, _, _, tree, _, _, _, _, _, _, _, _, _, _, tree, _],
             [_, _, _, _, _, _, tree, tree, _, _, _, _, tree, _, _, _]],
    RowNums = [3, 3, 3, 4, 2, 4, 2, 3, 4, 2, 5, 1, 6, 1, 5, 3],
    ColNums = [5, 2, 6, 0, 6, 1, 5, 2, 4, 1, 3, 3, 3, 3, 2, 5].

test8(test(Board, RowNums, ColNums)) :-
    Board = [[_, _, _, _, _, tree, _, _, tree, _, tree, _, tree, _, _, _],
             [_, _, tree, tree, _, _, _, tree, _, _, _, _, _, _, _, _],
             [_, _, tree, _, _, _, _, _, _, _, _, _, _, _, _, tree],
             [_, _, tree, _, _, tree, _, _, _, tree, _, _, _, tree, tree, _],
             [tree, _, _, _, _, _, tree, _, _, _, tree, _, _, _, _, _],
             [_, tree, _, _, tree, _, _, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, tree, _, _, _, _, _, tree, _, _, tree],
             [_, _, _, _, tree, _, _, _, _, tree, tree, _, _, _, tree, _],
             [_, tree, _, _, _, _, _, _, _, _, _, _, _, _, tree, _],
             [tree, _, _, _, _, tree, _, _, _, tree, _, _, _, _, tree, _],
             [_, _, _, _, _, _, _, _, _, tree, _, _, _, _, _, _],
             [_, tree, _, tree, tree, _, _, _, _, _, _, tree, _, _, _, _],
             [tree, _, tree, _, _, _, _, tree, _, _, _, _, _, _, tree, _],
             [_, _, _, _, _, _, _, _, _, _, tree, _, _, _, _, _],
             [tree, _, _, tree, _, tree, _, _, _, tree, _, _, tree, _, _, _],
             [_, _, _, _, _, _, tree, tree, tree, _, _, _, _, _, tree, _]],
    RowNums = [2, 5, 2, 2, 6, 1, 4, 2, 5, 3, 3, 4, 1, 4, 2, 5],
    ColNums = [4, 3, 2, 5, 2, 5, 2, 5, 1, 5, 2, 4, 2, 3, 2, 4].

test9(test(Board, RowNums, ColNums)) :-
    Board = [[_, tree, tree, _, _, _, _, _, tree, _, tree, _, tree, _, _, tree, _, _, _, _],
             [_, _, _, _, _, _, _, _, tree, _, _, _, _, _, _, _, _, _, _, tree],
             [_, tree, tree, _, _, tree, _, _, _, _, _, _, _, _, tree, _, tree, _, _, _],
             [_, _, _, _, _, _, _, _, _, tree, _, _, tree, _, _, _, _, _, _, _],
             [_, tree, _, _, _, tree, _, tree, _, _, _, _, _, _, tree, _, _, _, tree, _],
             [tree, _, _, _, _, tree, tree, _, _, _, tree, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, _, _, _, _, tree, _, _, _, tree, tree, tree, tree, _],
             [tree, _, tree, _, _, _, _, _, _, tree, _, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, tree, tree, _, _, tree, _, tree, _, _, _, _, tree, _],
             [_, tree, _, _, _, _, _, _, _, _, tree, _, _, tree, _, _, _, tree, _, _],
             [_, _, _, tree, tree, _, _, tree, _, _, _, _, _, _, _, _, _, _, _, _],
             [_, tree, _, tree, _, _, _, _, _, _, _, _, _, _, _, _, tree, _, tree, _],
             [_, _, _, _, _, _, _, _, _, tree, _, _, _, tree, _, _, _, _, _, _],
             [_, tree, tree, _, _, _, _, _, _, _, _, tree, tree, tree, _, tree, _, _, _, _],
             [_, _, _, tree, _, _, tree, _, _, _, _, _, _, _, _, _, _, tree, _, _],
             [_, _, tree, _, _, _, tree, _, _, tree, _, _, _, _, _, _, _, tree, _, _],
             [_, _, _, _, _, _, _, _, tree, _, _, _, _, tree, tree, tree, _, tree, tree, _],
             [_, tree, _, _, _, tree, _, _, _, _, _, _, _, _, _, _, _, _, _, _],
             [_, tree, _, _, tree, _, _, _, _, tree, _, tree, _, _, _, _, _, _, _, _],
             [_, _, _, tree, tree, _, _, tree, _, _, _, _, _, _, tree, _, _, tree, _, _]],
    RowNums = [3, 4, 4, 4, 5, 4, 4, 4, 5, 3, 3, 6, 2, 5, 3, 5, 2, 6, 3, 5],
    ColNums = [7, 2, 5, 3, 5, 1, 6, 3, 6, 1, 7, 1, 7, 1, 5, 5, 4, 3, 5, 3].

printBoard([]).
printBoard([Head|Tail]) :-
        write(Head), nl,
        printBoard(Tail).

runTest(Test) :-
    call(Test, test(Board, RowNums, ColNums)),
    isSolution(Board, RowNums, ColNums),
    printBoard(Board).
