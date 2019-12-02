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
% generateRow([gird], string, hide/show)
generateRow([], "", _).
generateRow([grid(_, true, _, _, _, _, _, _, _)|T], S, show) :-
    generateRow(T, NS, show),
    string_concat("[X]", NS, S), !.
generateRow([grid(_, true, _, _, _, _, _, _, _)|T], S, hide) :-
    generateRow(T, NS, hide),
    string_concat("[|]", NS, S), !.
generateRow([grid(_, _, _, _, _, true, _, _, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[F]", NS, S), !.
generateRow([grid(_, _, true, true, _, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[A]", NS, S), !.
generateRow([grid(_, _, true, _, true, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[A]", NS, S), !.
generateRow([grid(_, _, true, _, _, _, _, _, Num)|T], S, O) :-
    Num \= 0,     generateRow(T, NS, O),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S), !.
generateRow([grid(_, false, false, _, _, _, _, _, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[|]", NS, S), !.
generateRow([grid(_, _, true, _, _, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[ ]", NS, S), !.

% update the row of game board after clicking
% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
clickRow(_, [], []).
clickRow(Col, [grid((Row,Col), A, _, C, D, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, true, C, D, E, F, G, H)], T, GridRow), !.
clickRow(Col, [H|T], GridRow) :-
    clickRow(Col, T, Grids),
    append([H], Grids, GridRow), !.
% Click a grid and update the whole game board
click(_, _, [], []).
click(0, Col, [H|T], Board) :-
    clickRow(Col, H, NRow),
    append([NRow], T, Board),!.
click(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    click(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

% Return a list of indexes of grid that doesnt have mines or not flagged above the grid on row column
expandUp(_, _, [], []).
expandUp(Row, _, _, []) :-
    Row < 0, !.
expandUp(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    mined(Grid, 1), !.
expandUp(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    property(reached, Grid), !.
expandUp(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandUp(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(flagged, Grid), !.
expandUp(Row, Col, Board, List) :-
    NRow is Row - 1,
    expandUp(NRow, Col, Board, NList),
    append([(Row, Col)], NList, List), !.

% Return a list of indexes of grid that doesnt have mines or not flagged beneth the grid on row column
expandDown(_, _, [], []).
expandDown(Row, _, Board, []) :-
    length(Board, L),
    Row >= L, !.
expandDown(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    mined(Grid, 1), !.
expandDown(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    property(reached, Grid), !.
expandDown(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandDown(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(flagged, Grid), !.
expandDown(Row, Col, Board, List) :-
    NRow is Row + 1,
    expandDown(NRow, Col, Board, NList),
    append([(Row, Col)], NList, List), !.

expandLeft(_, _, [], []).
expandLeft(_, Col, _, []) :-
    Col < 0, !.
expandLeft(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    mined(Grid, 1), !.
expandLeft(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    property(reached, Grid), !.
expandLeft(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandLeft(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(flagged, Grid), !.
expandLeft(Row, Col, Board, List) :-
    NCol is Col - 1,
    expandLeft(Row, NCol, Board, NList),
    append([(Row, Col)], NList, List), !.

expandRight(_, _, [], []).
expandRight(_, Col, [H|_], []) :-
    length(H, L),
    Col >= L, !.
expandRight(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    mined(Grid, 1), !.
expandRight(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    property(reached, Grid), !.
expandRight(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandRight(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(flagged, Grid), !.
expandRight(Row, Col, Board, List) :-
    NCol is Col + 1,
    expandRight(Row, NCol, Board, NList),
    append([(Row, Col)], NList, List), !.

expandUDRL(Row, Col, Board, List) :- 
    expandUp(Row,Col,Board,L1), expandDown(Row,Col,Board,L2),
    expandLeft(Row,Col,Board,L3), expandRight(Row,Col,Board,L4),
    append(L1, L2, L5), append(L5, L3, L6),
    append(L6, L4, L7), sort(L7,List).

% Return a list of all grids around the grid on row, column that doesnt have mines
expandHelper([], _, []).
expandHelper([(X,Y)|T], Board, Lst) :-
    expandUDRL(X, Y, Board, NLst),
    expandHelper(T, Board, NNLst),
    append(NLst, NNLst, L1),
    sort(L1, Lst), !.

expand(_, [], _, []).
expand(_, [(X,Y)|T], Board, [(X,Y)|T]) :-
    findGrid(X, Y, Board, Grid),
    property(mined, Grid), !.
expand(Lst, [(X,Y)|T], Board, NewLst) :-
    expandHelper([(X,Y)|T],Board, ToExpand1),
    sort(ToExpand1, ToExpand),
    append([(X, Y)], Lst, Expanded1),
    sort(Expanded1, Expanded),
    subtract(ToExpand, Expanded, NL),
    expand(Expanded, NL, Board, L1),
    append([(X, Y)], L1, NewLst).

expandClick([], Board, Board).
expandClick([(X, Y)|T], Board, NBoard) :-
    click(X, Y, Board, Tmp),
    expandClick(T, Tmp, NBoard).

% generateBoard([GridRows], row_no, string)
generateBoard([], _, "", _).
generateBoard([H|T], Row, S, O) :-
    Row < 10, number_string(Row, RowS),
    string_concat(RowS, " ", S1),
    generateRow(H, S2, O),
    string_concat(S1, S2, S3),
    string_concat(S3, "\n", S4),
    NRow is Row + 1,
    generateBoard(T, NRow, S5, O),
    string_concat(S4, S5, S).
generateBoard([H|T], Row, S, O) :-
    Row >= 10, number_string(Row, RowS),
    generateRow(H, S1, O),
    string_concat(RowS, S1, S2),
    string_concat(S2, "\n", S3),
    NRow is Row + 1,
    generateBoard(T, NRow, S4, O),
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
property(location, grid((X, Y), _, _, _, _, _, _, _, _), X, Y).
property(mined, grid(_, true, _, _, _, _, _, _, _)).
property(reached, grid(_, _, true, _, _, _, _, _, _)).
property(reachedA, grid(_, _, _, true, _, _, _, _, _)).
property(reachedB, grid(_, _, _, _, true, _, _, _, _)).
property(flagged, grid(_, _, _, _, _, true, _, _, _)).
property(flaggedA, grid(_, _, _, _, _, _, true, _, _)).
property(flaggedB, grid(_, _, _, _, _, _, _, true, _)).
property(num, grid(_, _, _, _, _, _, _, _, Num), Num).

% Check if the grid with current coordinate is mined, if so return 1.
mineToOne(Row, Column, Board, X) :-
    findGrid(Row, Column, Board, Grid),
    mined(Grid, X), !.
mineToOne(_, _, _, 0).

% Get the number of mines around the grid
getGridNum(Grid, Board, Num) :-
    property(location, Grid, X, Y), X1 is X - 1, Y1 is Y - 1, X2 is X + 1, Y2 is Y + 1,
    mineToOne(X1, Y1, Board, N1), mineToOne(X1, Y, Board, N2), mineToOne(X1, Y2, Board, N3),
    mineToOne(X, Y1, Board, N4), mineToOne(X, Y2, Board, N5),
    mineToOne(X2, Y1, Board, N6), mineToOne(X2, Y, Board, N7), mineToOne(X2, Y2, Board, N8),
    Num is N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8.

newGridWithNumChanged(grid(A, B, C, D, E, F, G, H, _), grid(A, B, C, D, E, F, G, H, J), J).

% Update the column of a rows grids to show the number of mines around it
updateGridNumColumn([], _, []).
updateGridNumColumn([H|T], Board, NCol) :-
    getGridNum(H, Board, Num),
    newGridWithNumChanged(H, NGrid, Num),
    updateGridNumColumn(T, Board, NGrids),
    append([NGrid], NGrids, NCol).

% Get a new board with all grids num updated
updateGridNum([], _, []).
updateGridNum([H|T], Board, NBoard) :-
    updateGridNumColumn(H, Board, NRow),
    updateGridNum(T, Board, NRows),
    append([NRow], NRows, NBoard).

% TODO: DELETE!!!!!
testPrintBoard :-
    buildBoard(0, 0, 4, 4, [(0,0), (1,1)], Grids),
    updateGridNum(Grids, Grids, Board),
    printBoard(Board, hide),
    expand([],[(3,1)],Board, B),
    expandClick(B, Board, NBoard),
    printBoard(NBoard, show).

printBoard(Board, O) :-
    generateColumnCoord(Board, 0, ColCords),
    generateBoard(Board, 0, BoardS, O),
    write(ColCords),
    write(BoardS).

% getValidInput(X, Lower, Upper): Get an input X from user, and test if it is in [Lower, Upper]
getValidInput(X, Lower, Upper) :-
    read(X), integer(X), X >= Lower, X =< Upper, !.

getValidInput(X, Lower, Upper) :-
    X \= "Q", X \= "q",
    write("Please enter a valid input:\n"),
    getValidInput(X, Lower, Upper).

sizeDifficulty(5, 5, 0).
sizeDifficulty(9, 9, 1).
sizeDifficulty(18, 18, 2).
sizeDifficulty(24, 24, 3).

numMinesDiff(2, 0).
numMinesDiff(10, 1).
numMinesDiff(40, 2).
numMinesDiff(99, 3).

% Start the single game loop
startSingleGame :-
    write("Please enter your name: (No need to type dot)"), nl,
    read_string(user_input, "\n", "\r", _, _),
    read_string(user_input, "\n", "\r", _, Name),
    write(Name), nl,
    write("Please choose difficulty: (Q to quit)"), nl,
    write("0. Super Easy 1. Easy   2. Medium   3. Difficult"), nl,
    getValidInput(Difficult, 0, 3),
    sizeDifficulty(W, L, Difficult),
    numMinesDiff(N,Difficult),
    minesPositions(W, L, N, Positions),
    buildBoard(0, 0, W, L, Positions, Board),
    singleGame(Board, Difficult).

singleGame(Board, Difficult) :-
    updateGridNum(Board, Board, NBoard),
    \+ printWinGame(NBoard, Difficult),
    \+ printLoseGame(NBoard, Difficult),
    printBoard(NBoard, show),           % show means show mines, hide will hide mines
    sizeDifficulty(W, L, Difficult),
    NW is W - 1, NL is L - 1,
    write("Please choose a Row: (Q to quit)"), nl,
    getValidInput(X, 0, NW),
    write("Please choose a Column: (Q to quit)"), nl,
    getValidInput(Y, 0, NL),
    expand([],[(X,Y)],NBoard, B),
    expandClick(B, NBoard, NNBoard),
    singleGame(NNBoard, Difficult).

% Create ranking files if there are no such files.
createFileIfNonExist(FileName) :-
    not(exists_file(FileName)),
    open(FileName,write,OS),
    X = "",
    write(OS,X),
    close(OS), !.
createFileIfNonExist(FileName) :-
    exists_file(FileName).

linesToRecords([], []).
linesToRecords([H|T], Records) :-
    split_string(H, " ","", Lst),
    nth0(0, Lst, E1),
    nth0(1, Lst, E2),
    number_string(Time, E2),
    append([(Time, E1)], NRecords, Records),
    linesToRecords(T, NRecords).

findSpaceNum(N, String) :-
    Tmp is 9-N, Tmp > 0,
    NN is N + 1,
    findSpaceNum(NN, S),
    string_concat(" ", S, String), !.
findSpaceNum(_, "").

createRank([], _, "").
createRank([(Time, Name)|T], N, RankS) :-
    number_string(N, NS), string_concat(NS, ". ", S1),
    string_concat(S1, Name, S2),  string_length(Name, Length),
    findSpaceNum(Length, Space), string_concat(S2, Space, S3),
    number_string(Time, TimeS),
    string_concat(S3, TimeS, S4), string_concat(S4, "s\n", S5),
    NN is N + 1,
    createRank(T, NN, S),
    string_concat(S5, S, RankS).

difficultFileAndTitle("SuperEasy", "Super Easy").
difficultFileAndTitle(X, X).

checkWinGame([], _, _, _, _).
checkWinGame([H|T], C, Row, Column):-
    C < Row, 
    checkWinRow(H, 0, Column),
    NC is C + 1,
    checkWinGame(T, NC, Row, Column).

checkWinRow([], _, _).
checkWinRow([H|T], C, Column):-
    C < Column,
    \+ reachedMine(H),
    NC is C + 1,
    checkWinRow(T, NC, Column).

% checkLoseGame([], _, _, _).
checkLoseGame([H|T], C, Row, Column):-
    C < Row, 
    NC is C + 1,
    (checkLoseRow(H, 0, Column);
    checkLoseGame(T, NC, Row, Column)).

% checkLoseRow([], _, _).
checkLoseRow([H|T], C, Column):-
    C < Column,
    NC is C + 1,
    (reachedMine(H); checkLoseRow(T, NC, Column)).

reachedMine(Grid):-
    property(reached, Grid),
    property(mined, Grid).

printWinGame(Board, Difficult):-
    sizeDifficulty(R, C, Difficult),
    checkWinGame(Board, 0, R, C),
    write("You Win!!!"), nl.

printLoseGame(Board, Difficult):-
    sizeDifficulty(R, C, Difficult),
    checkLoseGame(Board, 0, R, C),
    write("You Lose..."), nl.
    

generateRanking(DifficultS) :-
    string_concat(DifficultS, ".txt", File),
    open(File, read, Str),
    read_string(Str, "", "\r", _, X), !,
    X = "",
    difficultFileAndTitle(DifficultS, TitleS),
    string_concat("  ", TitleS, Title),
    write(Title), nl,
    write("----------------"), nl,
    write(""), nl,
    close(Str), !.

generateRanking(DifficultS) :-
    string_concat(DifficultS, ".txt", File),
    open(File, read, Str),
    read_string(Str, "", "\r", _, X), !,
    split_string(X, "\n", "", Lst),
    linesToRecords(Lst, Records),
    sort(Records, SortRecords),
    difficultFileAndTitle(DifficultS, TitleS),
    string_concat("  ", TitleS, Title),
    write(Title), nl,
    write("----------------"), nl,
    createRank(SortRecords, 1, S),
    write(S), nl,
    close(Str).

userRanking(1) :-
    createFileIfNonExist("SuperEasy.txt"),
    createFileIfNonExist("Easy.txt"),
    createFileIfNonExist("Medium.txt"),
    createFileIfNonExist("Difficult.txt"),
    generateRanking("SuperEasy"),
    generateRanking("Easy"),
    generateRanking("Medium"),
    generateRanking("Difficult"),
    main, !.

userRanking(2).

main :-
    write("1. Check Ranking 2. Play Game (q to Quit)"), nl,
    getValidInput(X, 1, 2),
    userRanking(X),
    startSingleGame ,!.