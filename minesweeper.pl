firTup((X,_), X).       % First element in the tuple
secTup((_,X), X).       % Second element in the tuple

% mytake(N_elements, List, New_List)
mytake(0, _, []).
mytake(N, [H|T], R) :-
    append([H], R1, R),
    NN is N-1,
    mytake(NN, T, R1).

% allPairs(row_indexes, col_indexes, Pairs)
allPairs([], _, []).
allPairs([H|T], L, P) :-
    allPairsHelper(H, L, P1),
    append(P1, P2, P),
    allPairs(T, L, P2).

allPairsHelper(_, [], []).
allPairsHelper(A, [H|T], P) :- 
    append([(A,H)], P1, P),
    allPairsHelper(A, T, P1).

% minesPositions(Width, Length, Num, Positions)
minesPositions(W, L, N, P) :-
    NW is W-1, NL is L-1,
    numlist(0, NW, WL), numlist(0, NL, LL),
    allPairs(WL, LL, P1),
    random_permutation(P1, NP),
    mytake(N, NP, P).

% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
% initRow(row, column, length, [mines_locations], [grids_of_this_row])
initRow(_, Col, Col, _, []).
initRow(Row, Col, Len, Mines, Grids) :-
    Col < Len, member((Row,Col), Mines), append([grid((Row,Col), true, false, false, false, false, false, false, 0)], NGrids, Grids),
    NCol is Col+1,
    initRow(Row, NCol, Len, Mines, NGrids).
initRow(Row, Col, Len, Mines, Grids) :-
    Col < Len, not(member((Row,Col), Mines)), append([grid((Row,Col), false, false, false, false, false, false, false, 0)], NGrids, Grids),
    NCol is Col+1,
    initRow(Row, NCol, Len, Mines, NGrids).

% buildBoard (row, column, width, length, [mines_locations], [grids_of_this_row])
buildBoard(Row, _, Row, _, _, []).
buildBoard(Row, Col, Wid, Len, Mines, Grids) :-
    Row < Wid, append([GridRow], GridRows, Grids),
    NRow is Row + 1,
    initRow(Row, Col, Len, Mines, GridRow),
    buildBoard(NRow, Col, Wid, Len, Mines, GridRows).

% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
% generateRow([gird], string)
generateRow([], "").
generateRow([grid(_, true, _, _, _, _, _, _, _)|T], S) :-
    generateRow(T, NS),
    string_concat("[X]", NS, S), !.
generateRow([grid(_, _, _, _, _, true, _, _, _)|T], S) :-
    generateRow(T, NS),
    string_concat("[F]", NS, S), !.
generateRow([grid(_, _, true, true, _, _, _, _, 0)|T], S) :-
    generateRow(T, NS),
    string_concat("[A]", NS, S), !.
generateRow([grid(_, _, true, _, true, _, _, _, 0)|T], S) :-
    generateRow(T, NS),
    string_concat("[A]", NS, S), !.
generateRow([grid(_, _, true, _, _, _, _, _, Num)|T], S) :-
    Num \= 0, generateRow(T, NS),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S), !.
generateRow([grid(_, false, _, _, _, _, _, _, _)|T], S) :-
    generateRow(T, NS),
    string_concat("[|]", NS, S), !.

% update the row of game board after clicking
% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
clickRow(_, [], []).
clickRow(Col, [grid((Row,Col), A, _, C, D, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, true, C, D, E, F, G, H)], T, GridRow), !.
clickRow(Col, [H|T], GridRow) :-
    clickRow(Col, T, Grids),
    append([H], Grids, GridRow), !.
% Click a grid and update the whole game board (Single player mode)
click(_, _, [], []).
click(0, Col, [H|T], Board) :-
    clickRow(Col, H, NRow),
    append([NRow], T, Board),!.
click(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    click(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

% generateBoard([GridRows], row_no, string)
generateBoard([], _, "").
generateBoard([H|T], Row, S) :-
    Row < 10, number_string(Row, RowS),
    string_concat(RowS, " ", S1),
    generateRow(H, S2),
    string_concat(S1, S2, S3),
    string_concat(S3, "\n", S4),
    NRow is Row + 1,
    generateBoard(T, NRow, S5),
    string_concat(S4, S5, S).
generateBoard([H|T], Row, S) :-
    Row >= 10, number_string(Row, RowS),
    generateRow(H, S1),
    string_concat(RowS, S1, S2),
    string_concat(S2, "\n", S3),
    NRow is Row + 1,
    generateBoard(T, NRow, S4),
    string_concat(S3, S4, S).

% generateColumnCoord([GridRows], Column, String)
generateColumnCoord([], _, "").
generateColumnCoord([H|T], 0, S) :-
    generateColumnCoord([H|T], 1, NS),
    string_concat("   0", NS, S).
generateColumnCoord([H|T], Col, S) :-
    length(H, L), Col < L, Col < 10, Col \= 0,
    number_string(Col, ColS),
    string_concat("  ", ColS, SS),
    NCol is Col + 1,
    generateColumnCoord([H|T], NCol, NS),
    string_concat(SS, NS, S).
generateColumnCoord([H|T], Col, S) :-
    length(H, L), Col < L, Col >= 10,
    number_string(Col, ColS),
    string_concat(" ", ColS, SS),
    NCol is Col + 1,
    generateColumnCoord([H|T], NCol, NS),
    string_concat(SS, NS, S).
generateColumnCoord([H|_], Col, "\n") :-
    length(H, L), L = Col.

% Return the grid with given coordinate
findGrid(Row, Column, [H|T], Grid) :-
    Row >= 0, length([H|T], L),
    NL is L - 1,
    Row =< NL,
    Column >= 0, length(H, W),
    NW is W - 1,
    Column =< NW,
    nth0(Row, [H|T], BoardRow),
    nth0(Column, BoardRow, Grid).

% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
% mined(Grid, X) returns true if Grid has mine on it
mined(grid(_, true, _, _, _, _, _, _, _), 1).
mined(grid(_, false, _, _, _, _, _, _, _), 0).
location(grid((X, Y), _, _, _, _, _, _, _, _), X, Y).

% Check if the grid with current coordinate is mined, if so return 1.
mineToOne(Row, Column, Board, X) :-
    findGrid(Row, Column, Board, Grid),
    mined(Grid, X), !.
mineToOne(_, _, _, 0).

% Get the number of mines around the grid
getGridNum(Grid, Board, Num) :-
    location(Grid, X, Y), X1 is X - 1, Y1 is Y - 1, X2 is X + 1, Y2 is Y + 1,
    mineToOne(X1, Y1, Board, N1), mineToOne(X1, Y, Board, N2), mineToOne(X1, Y2, Board, N3),
    mineToOne(X, Y1, Board, N4), mineToOne(X, Y2, Board, N5),
    mineToOne(X2, Y1, Board, N6), mineToOne(X2, Y, Board, N7), mineToOne(X2, Y2, Board, N8),
    Num is N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8.

newGridWithNumChanged(grid(A, B, C, D, E, F, G, H, _), grid(A, B, C, D, E, F, G, H, J), J).

% Update the column of a row's grids to show the number of mines around it
updateGridNumColumn([], _, []).
updateGridNumColumn([H|T], Board, NCol) :-
    getGridNum(H, Board, Num),
    newGridWithNumChanged(H, NGrid, Num),
    updateGridNumColumn(T, Board, NGrids),
    append([NGrid], NGrids, NCol).

% Get a new board with all grids' num updated
updateGridNum([], _, []).
updateGridNum([H|T], Board, NBoard) :-
    updateGridNumColumn(H, Board, NRow),
    updateGridNum(T, Board, NRows),
    append([NRow], NRows, NBoard).

% TODO: DELETE!!!!!
testFindGrid :-
    buildBoard(0, 0, 4, 4, [(0,0), (1,1), (2,2), (3,3)], Grids),
    updateGridNum(Grids, Grids, Board),
    click(0, 1, Board, X),
    printBoard(X).

printBoard(Board) :-
    generateColumnCoord(Board, 0, ColCords),
    generateBoard(Board, 0, BoardS),
    write(ColCords),
    write(BoardS).

% getValidInput(X, Lower, Upper): Get an input X from user, and test if it is in [Lower, Upper]
getValidInput(X, Lower, Upper) :-
    read(X), integer(X), X >= Lower, X =< Upper.

getValidInput(X, Lower, Upper) :-
    write("Please enter a valid input:\n"),
    getValidInput(X, Lower, Upper).

sizeDifficulty(5, 5, 0).
sizeDifficulty(9, 9, 1).
sizeDifficulty(18, 18, 2).
sizeDifficulty(24,24, 3).

numMinesDiff(2, 0).
numMinesDiff(10, 1).
numMinesDiff(40, 2).
numMinesDiff(99, 3).

startSingleGame :-
    write("Please choose difficulty:"), nl,
    write("0. Super Easy 1. Easy   2. Medium   3. Difficult"), nl,
    getValidInput(Difficult, 0, 3),
    sizeDifficulty(W, L, Difficult),
    numMinesDiff(N,Difficult),
    minesPositions(W, L, N, Positions),
    buildBoard(0, 0, W, L, Positions, Board),
    singleGame(Board).

singleGame(Board) :-
    printBoard(Board).

main :-
    startSingleGame.