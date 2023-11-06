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

% allSomething: List, Something
allSomething([], _).
allSomething([A|T], A) :-
    allSomething(T, A).

% will not allow us to proceed to the next row unless the tent count in
% this row is satisfied
% solveNextPosition: Board, RowNums, ColNums, Connected, CurRow, CurColumn
solveNextPosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn) :-
    nth0(0, Board, FirstRow),
    length(Board, NumRows),
    length(FirstRow, NumCols),
    RowLimit is NumRows - 1,
    ColLimit is NumCols - 1,
    ((CurColumn < ColLimit, % increment column
      NextColumn is CurColumn + 1,
      solvePosition(Board, RowNums, ColNums, Connected, CurRow, NextColumn));
     (CurColumn == ColLimit, % increment row
      CurRow < RowLimit,
      nth0(CurRow, RowNums, 0), % this row is satisfied
      NextRow is CurRow + 1,
      solvePosition(Board, RowNums, ColNums, Connected, NextRow, 0));
     (CurColumn == ColLimit, % complete
      CurRow == RowLimit,
      nth0(RowLimit, RowNums, 0), % this row is satisfied
      allSomething(ColNums, 0))). % all columns happy

decNum(In, Out) :-
    In > 0,
    Out is In - 1.

% replaceList: List, Position, With, NewList
replaceList([_|T], 0, With, [With|T]).
replaceList([H|T], Pos, With, [H|Rest]) :-
    Pos > 0,
    NewPos is Pos - 1,
    replaceList(T, NewPos, With, Rest).

% decrementTentCount: RowNums, ColNums, RowPlace, ColPlaced, NewRowNums, NewColNums
decrementTentCount(RowNums, ColNums, Row, Col, NewRowNums, NewColNums) :-
    nth0(Row, RowNums, OldRowTentNum),
    nth0(Col, ColNums, OldColTentNum),
    decNum(OldRowTentNum, NewRowTentNum),
    decNum(OldColTentNum, NewColTentNum),
    replaceList(RowNums, Row, NewRowTentNum, NewRowNums),
    replaceList(ColNums, Col, NewColTentNum, NewColNums).

% connected: [pair(Row, Col)]
% connected represents the trees that have connections on them.
% An additional constraint is that we cannot connect two tents to
% the same tree.
%
% solvePosition: Board, RowNums, ColNums, Connected, CurRow, CurColumn
solvePosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn) :-
    % Case #1: we have a tree here.  Just move on
    getSquare(Board, CurRow, CurColumn, [Tree]),
    Tree == tree,
    solveNextPosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn).
solvePosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn) :-
    % Case #2: try to place a tent.  Tents can only be placed
    % if there is a tree horizontally or vertically nearby, and
    % there are no tends horizontally, vertically, or diagonally    
    getSquare(Board, CurRow, CurColumn, [tent]),
    decrementTentCount(RowNums, ColNums, CurRow, CurColumn, NewRowNums, NewColNums),
    \+ tentNearby(Board, CurRow, CurColumn),
    treeSquares(Board, CurRow, CurColumn, TreePositions),
    member(ConnectHere, TreePositions),
    \+ member(ConnectHere, Connected),
    solveNextPosition(Board, NewRowNums, NewColNums, [ConnectHere|Connected], CurRow, CurColumn).
solvePosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn) :-
    % Case #3: place empty.  Always legal
    getSquare(Board, CurRow, CurColumn, [empt]),
    solveNextPosition(Board, RowNums, ColNums, Connected, CurRow, CurColumn).
    
% isSolution: Board, RowNums, ColNums
isSolution(Board, RowNums, ColNums) :-
    solvePosition(Board, RowNums, ColNums, [], 0, 0).

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
