
%        Prolog Minesweeper
%--------------------------------------------------------------

%        Main Function
%--------------------------------------------------------------
%--------------------------------------------------------------

main :-
    write("To check score: 1. \nTo play Single Game:   2. \nTo play Multiple Game:   3. \nTo Quit:       4."), nl,
    getValidInput(X, 1, 4), 
    returnIf4(X).

returnIf4(X) :-
    createFileIfNonExist("SuperEasy.txt"),
    createFileIfNonExist("Easy.txt"),
    createFileIfNonExist("Medium.txt"),
    createFileIfNonExist("Difficult.txt"),
    userRanking(X), !.
returnIf4(4).

%        Gameloop
%--------------------------------------------------------------
%--------------------------------------------------------------

% Start the single game loop
startSingleGame :-
    write("Please enter your name: (No need to type dot)"), nl,
    read_string(user_input, "\n", "\r", _, _),
    read_string(user_input, "\n", "\r", _, Name),
    write("Please choose difficulty:"), nl,
    write("Super Easy: 0.\nEasy:       1.\nMedium:     2.\nDifficult:  3."), nl,
    getValidInput(Difficult, 0, 3),
    sizeDifficulty(W, L, Difficult),
    numMinesDiff(N,Difficult),
    minesPositions(W, L, N, Positions),
    buildBoard(0, 0, W, L, Positions, Board),
    updateGridNum(Board, Board, NBoard),
    get_time(StartTime),
    singleGame(NBoard, Difficult, Name, StartTime), !.

singleGame(Board, Difficult, Name, StartTime) :-
    winGame(Board),
    write("You Win!!!"), nl, 
    printBoard(Board, show),           % show means show mines, hide will hide mines
    writeRecords(Name, Difficult, StartTime), 
    write("Do you wish to continue?\nYes:     0.  \nNo:      1."), nl,
    getValidInput(Input, 0, 1),
    continueGame(Input), !.

singleGame(Board, Difficult, _, _) :-
    printLoseGame(Board, Difficult),
    printBoard(Board, show),    
    write("Do you wish to continue?\nYes:     0.  \nNo:      1."), nl,
    getValidInput(Input, 0, 1),
    continueGame(Input), !.

singleGame(Board, Difficult, Name, StartTime) :-
    sizeDifficulty(W, L, Difficult),
    printBoard(Board, hide),           % show means show mines, hide will hide mines
    NW is W - 1, NL is L - 1,
    write("Set/Cancel a flag? 1 yes, 0 no"), nl,
    getValidInput(Input, 0, 1),
    dealWithInput(Input, NW, NL, Board, NNBoard),
    singleGame(NNBoard, Difficult, Name, StartTime), !.

startMultiGame :-
    write("Please choose difficulty:"), nl,
    write("Super Easy: 0.\nEasy:       1.\nMedium:     2.\nDifficult:  3."), nl,
    getValidInput(Difficult, 0, 3),
    sizeDifficulty(W, L, Difficult),
    numMinesDiff(N,Difficult),
    minesPositions(W, L, N, Positions),
    buildBoard(0, 0, W, L, Positions, Board),
    updateGridNum(Board, Board, NBoard),
    multiGame(NBoard, Difficult, 1), !.

multiGame(Board, _, _) :-
    winGame(Board),
    write("ALL Clear!!!"), nl, 
    printBoard(Board, show),           % show means show mines, hide will hide mines
    pScore(Board, A, B),
    whoWins(A, B),
    write("Do you wish to continue?\nYes:     0.  \nNo:      1."), nl,
    getValidInput(Input, 0, 1),
    continueGame(Input), !.

multiGame(Board, Difficult, Round) :-
    sizeDifficulty(R, C, Difficult),
    checkLoseGame(Board, 0, R, C, 0),
    printBoard(Board, show),    
    whoLose(Round),
    write("Do you wish to continue?\nYes:     0.  \nNo:      1."), nl,
    getValidInput(Input, 0, 1),
    continueGame(Input), !.

multiGame(Board, Difficult, 1) :-
    sizeDifficulty(W, L, Difficult),
    printBoard(Board, hide), 
    pScore(Board, _, _),          % show means show mines, hide will hide mines
    NW is W - 1, NL is L - 1,
    write("A Set a flag?\nYes:     1.  \nNo:      0."), nl,
    getValidInput(Input, 0, 1),
    dealWithInputA(Input, NW, NL, Board, NBoard),
    multiGame(NBoard, Difficult, 2), !.

multiGame(Board, Difficult, 2) :-
    sizeDifficulty(W, L, Difficult),
    printBoard(Board, hide), 
    pScore(Board, _, _),          % show means show mines, hide will hide mines
    NW is W - 1, NL is L - 1,
    write("B Set a flag?\nYes:     1.  \nNo:      0."), nl,
    getValidInput(Input, 0, 1),
    dealWithInputB(Input, NW, NL, Board, NBoard), 
    multiGame(NBoard, Difficult, 1), !.

whoLose(1):-
    write("Player B clicked on mine, B lose..."), nl.
whoLose(2):-
    write("Player A clicked on mine, A lose..."), nl.

continueGame(0) :- main, !.
continueGame(1).

userRanking(1) :-
    generateRanking("SuperEasy"),
    generateRanking("Easy"),
    generateRanking("Medium"),
    generateRanking("Difficult"),
    main, !.

userRanking(2) :- startSingleGame, !.
userRanking(3) :- startMultiGame.

%        Gameplay Functions
%--------------------------------------------------------------
%--------------------------------------------------------------

% buildBoard (row, column, width, length, [mines_locations], [grids_of_this_row])
buildBoard(Row, _, Row, _, _, []).
buildBoard(Row, Col, Wid, Len, Mines, Grids) :-
    Row < Wid, append([GridRow], GridRows, Grids),
    NRow is Row + 1,
    initRow(Row, Col, Len, Mines, GridRow),
    buildBoard(NRow, Col, Wid, Len, Mines, GridRows).

dealWithInput(0, NW, NL, NBoard, NNBoard) :-
    write("Please choose a Row: "), nl,
    getValidInput(X, 0, NW),
    write("Please choose a Column: "), nl,
    getValidInput(Y, 0, NL),
    expand([],[(X,Y)],NBoard, B),
    expandClick(B, NBoard, NNBoard), !.

dealWithInput(1, NW, NL, NBoard, NNBoard) :-
    write("Flag Please choose a Row: "), nl,
    getValidInput(X1, 0, NW),
    write("Flag Please choose a Column: "), nl,
    getValidInput(Y1, 0, NL),
    flag(X1, Y1, NBoard, NNBoard), !.

dealWithInputA(0, NW, NL, NBoard, NNBoard) :-
    write("A Please choose a Row: "), nl,
    getValidInput(X, 0, NW),
    write("A Please choose a Column: "), nl,
    getValidInput(Y, 0, NL),
    expand([],[(X,Y)],NBoard, B),
    expandClickA(B, NBoard, NNBoard),!.

dealWithInputA(1, NW, NL, NBoard, NNBoard) :-
    write("A Flag Please choose a Row: "), nl,
    getValidInput(X1, 0, NW),
    write("A Flag Please choose a Column: "), nl,
    getValidInput(Y1, 0, NL),
    flagA(X1, Y1, NBoard, NNBoard), !.

dealWithInputB(0, NW, NL, NBoard, NNBoard) :-
    write("B Please choose a Row: "), nl,
    getValidInput(X, 0, NW),
    write("B Please choose a Column: "), nl,
    getValidInput(Y, 0, NL),
    expand([],[(X,Y)],NBoard, B),
    expandClickB(B, NBoard, NNBoard), !.

dealWithInputB(1, NW, NL, NBoard, NNBoard) :-
    write("B Flag Please choose a Row: "), nl,
    getValidInput(X1, 0, NW),
    write("B Flag Please choose a Column: "), nl,
    getValidInput(Y1, 0, NL),
    flagB(X1, Y1, NBoard, NNBoard), !.

% Click a grid and update the whole game board
click(_, _, [], []).
click(0, Col, [H|T], Board) :-
    clickRow(Col, H, NRow),
    append([NRow], T, Board),!.
click(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    click(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

clickA(_, _, [], []).
clickA(0, Col, [H|T], Board) :-
    clickRowA(Col, H, NRow),
    append([NRow], T, Board),!.
clickA(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    clickA(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

clickB(_, _, [], []).
clickB(0, Col, [H|T], Board) :-
    clickRowB(Col, H, NRow),
    append([NRow], T, Board),!.
clickB(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    clickB(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

flagA(_, _, [], []).
flagA(-1, -1, Board, Board).
flagA(0, Col, [H|T], Board) :-
    flagRowA(Col, H, NRow),
    append([NRow], T, Board),!.
flagA(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    flagA(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

flagB(_, _, [], []).
flagB(-1, -1, Board, Board).
flagB(0, Col, [H|T], Board) :-
    flagRowB(Col, H, NRow),
    append([NRow], T, Board),!.
flagB(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    flagB(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

flag(_, _, [], []).
flag(-1, -1, Board, Board).
flag(0, Col, [H|T], Board) :-
    flagRow(Col, H, NRow),
    append([NRow], T, Board),!.
flag(Row, Col, [H|T], Board) :-
    NRow is Row - 1,
    flag(NRow, Col, T, Rows),
    append([H], Rows, Board), !.

expandClick([], Board, Board).
expandClick([(X, Y)|T], Board, NBoard) :-
    click(X, Y, Board, Tmp),
    expandClick(T, Tmp, NBoard).

expandClickA([], Board, Board).
expandClickA([(X, Y)|T], Board, NBoard) :-
    clickA(X, Y, Board, Tmp),
    expandClickA(T, Tmp, NBoard).

expandClickB([], Board, Board).
expandClickB([(X, Y)|T], Board, NBoard) :-
    clickB(X, Y, Board, Tmp),
    expandClickB(T, Tmp, NBoard).

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

% Get a new board with all grids num updated
updateGridNum([], _, []).
updateGridNum([H|T], Board, NBoard) :-
    updateGridNumColumn(H, Board, NRow),
    updateGridNum(T, Board, NRows),
    append([NRow], NRows, NBoard).

winGame(Board) :-
    winBoardCheck(Board), !.
winGame(Board) :-
    allMinesOnFlag(Board), !.

printBoard(Board, O) :-
    generateColumnCoord(Board, 0, ColCords),
    generateBoard(Board, 0, BoardS, O),
    write(ColCords),
    write(BoardS).

printLoseGame(Board, Difficult):-
    sizeDifficulty(R, C, Difficult),
    checkLoseGame(Board, 0, R, C, N),
    N is 0,
    write("You Lose..."), nl, !.

printLoseGame(Board, Difficult):-
    sizeDifficulty(R, C, Difficult),
    checkLoseGame(Board, 0, R, C, N),
    N is 1,
    write("Player A Lose..."), nl, !.

printLoseGame(Board, Difficult):-
    sizeDifficulty(R, C, Difficult),
    checkLoseGame(Board, 0, R, C, N),
    N is 2,
    write("Player B Lose..."), nl, !.

%        Helper Functions
%--------------------------------------------------------------
%--------------------------------------------------------------

firTup((X,_), X).       % First element in the tuple
secTup((_,X), X).       % Second element in the tuple

% mytake(N_elements, List, New_List)
mytake(_, [], []).
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

% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
% generateRow([gird], string, hide/show)
generateRow([], "", _).
generateRow([grid(_, _, _, _, _, true, _, _, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[F]", NS, S), !.
generateRow([grid(_, _, _, _, _, _, true, _, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[P]", NS, S), !.
generateRow([grid(_, _, _, _, _, _, _, true, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[Y]", NS, S), !.
generateRow([grid(_, true, _, _, _, _, _, _, _)|T], S, show) :-
    generateRow(T, NS, show),
    string_concat("[X]", NS, S), !.
generateRow([grid(_, _, true, _, _, _, _, _, Num)|T], S, O) :-
    Num \= 0,     generateRow(T, NS, O),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S), !.
generateRow([grid(_, _, _, true, _, _, _, _, Num)|T], S, O) :-
    Num \= 0,     generateRow(T, NS, O),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S), !.
generateRow([grid(_, _, _, _, true, _, _, _, Num)|T], S, O) :-
    Num \= 0,     generateRow(T, NS, O),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S), !.
generateRow([grid(_, _, _, true, _, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[A]", NS, S), !.
generateRow([grid(_, _, _, _, true, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[B]", NS, S), !.
generateRow([grid(_, _, true, _, _, _, _, _, 0)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[ ]", NS, S), !.
generateRow([grid(_, true, _, _, _, _, _, _, _)|T], S, hide) :-
    generateRow(T, NS, hide),
    string_concat("[|]", NS, S), !.
generateRow([grid(_, false, false, _, _, _, _, _, _)|T], S, O) :-
    generateRow(T, NS, O),
    string_concat("[|]", NS, S), !.


% update the row of game board after clicking
% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
clickRow(_, [], []).
clickRow(Col, [grid((Row,Col), A, _, C, D, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, true, C, D, E, F, G, H)], T, GridRow), !.
clickRow(Col, [H|T], GridRow) :-
    clickRow(Col, T, Grids),
    append([H], Grids, GridRow), !.

clickRowA(_, [], []).
clickRowA(Col, [grid((Row,Col), A, B, C, true, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, true, E, F, G, H)], T, GridRow), !.
clickRowA(Col, [grid((Row,Col), A, _, _, false, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, true, true, false, E, F, G, H)], T, GridRow), !.
clickRowA(Col, [H|T], GridRow) :-
    clickRowA(Col, T, Grids),
    append([H], Grids, GridRow), !.
% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
clickRowB(_, [], []).
clickRowB(Col, [grid((Row,Col), A, B, true, D, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, true, D, E, F, G, H)], T, GridRow), !.
clickRowB(Col, [grid((Row,Col), A, _, false, _, E, F, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, true, false, true, E, F, G, H)], T, GridRow), !.
clickRowB(Col, [H|T], GridRow) :-
    clickRowB(Col, T, Grids),
    append([H], Grids, GridRow), !.

winBoardCheck([]).
winBoardCheck([H|T]) :-
    winRowCheck(H), winBoardCheck(T).

% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
flagRow(_, [], []).
flagRow(Col, [grid((Row,Col), A, B, C, D, E, F, G, H)|T], GridRow) :-
    neg(E, NE),
    append([grid((Row,Col), A, B, C, D, NE, F, G, H)], T, GridRow), !.
flagRow(Col, [H|T], GridRow) :-
    flagRow(Col, T, Grids),
    append([H], Grids, GridRow), !.

flagRowA(_, [], []).
flagRowA(Col, [grid((Row,Col), A, B, C, D, false, _, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, true, true, G, H)], T, GridRow), !.
flagRowA(Col, [grid((Row,Col), A, B, C, D, true, true, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, false, false, G, H)], T, GridRow), !.
flagRowA(Col, [grid((Row,Col), A, B, C, D, true, F, true, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, true, F, true, H)], T, GridRow), !.
flagRowA(Col, [H|T], GridRow) :-
    flagRowA(Col, T, Grids),
    append([H], Grids, GridRow), !.

flagRowB(_, [], []).
flagRowB(Col, [grid((Row,Col), A, B, C, D, false, F, _, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, true, F, true, H)], T, GridRow), !.
flagRowB(Col, [grid((Row,Col), A, B, C, D, true, F, true, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, false, F, false, H)], T, GridRow), !.
flagRowB(Col, [grid((Row,Col), A, B, C, D, true, true, G, H)|T], GridRow) :-
    append([grid((Row,Col), A, B, C, D, true, true, G, H)], T, GridRow), !.
flagRowB(Col, [H|T], GridRow) :-
    flagRowB(Col, T, Grids),
    append([H], Grids, GridRow), !.

% Return a list of indexes of grid that doesnt have mines or not flagged above the grid on row column
expandUp(_, _, [], []).
expandUp(Row, _, _, []) :-
    Row < 0, !.
expandUp(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    mined(Grid, 1), !.
expandUp(Row, Col, Board, []) :-
    findGrid(Row, Col, Board, Grid),
    (property(reached, Grid); property(reachedA, Grid); property(reachedB, Grid)), !.
expandUp(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandUp(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    (property(flagged, Grid); property(flaggedA, Grid); property(flaggedB, Grid)), !.
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
    (property(reached, Grid); property(reachedA, Grid); property(reachedB, Grid)), !.
expandDown(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandDown(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    (property(flagged, Grid); property(flaggedA, Grid); property(flaggedB, Grid)), !.
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
    (property(reached, Grid); property(reachedA, Grid); property(reachedB, Grid)), !.
expandLeft(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandLeft(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    (property(flagged, Grid); property(flaggedA, Grid); property(flaggedB, Grid)), !.
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
    (property(reached, Grid); property(reachedA, Grid); property(reachedB, Grid)), !.
expandRight(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    property(num, Grid, Num),
    Num \= 0, !.
expandRight(Row, Col, Board, [(Row, Col)]) :-
    findGrid(Row, Col, Board, Grid),
    (property(flagged, Grid); property(flaggedA, Grid); property(flaggedB, Grid)), !.
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

calcScore([], 0, 0).
calcScore([H|T], NS, NC) :-
    calcScoreRow(H, S, C),
    calcScore(T, A, B),
    NS is A + S,
    NC is B + C.
    
% Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)
calcScoreRow([], 0, 0).
calcScoreRow([grid(_, true, _, _, _, _, true, _, _)|T], NS, NC) :-
    calcScoreRow(T, S, NC),
    NS = S + 10.
calcScoreRow([grid(_, true, _, _, _, _, _, true, _)|T], NS, NC) :-
    calcScoreRow(T, NS, C),
    NC = C + 10.
calcScoreRow([grid(_, false, _, true, false, _, _, _, _)|T], NS, NC) :-
    calcScoreRow(T, S, NC),
    NS = S + 1.
calcScoreRow([grid(_, false, _, false, true, _, _, _, _)|T], NS, NC) :-
    calcScoreRow(T, NS, C),
    NC = C + 1.
calcScoreRow([_|T], S, C) :-
    calcScoreRow(T, S, C).

pScore(Board, A, B):-
    calcScore(Board, A, B),
    number_string(A, AA),
    number_string(B, BB),
    write("A Score: "),
    write(AA), nl,
    write("B Score: "),
    write(BB), nl.

whoWins(A,B) :-
    A > B, write("Player A wins!!!"), nl.
whoWins(A,B) :-
    A = B, write("Draw!!!"), nl.
whoWins(A,B):-
    A < B, write("Player B wins!!!"), nl.

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

winRowCheck([]).
winRowCheck([H|T]) :-
    property(mined, H), (not(property(reached, H)); not(property(reachedA, H)); not(property(reachedB, H))), winRowCheck(T), !.
winRowCheck([H|T]) :-
    not(property(mined, H)), (property(reached, H); property(reachedA, H); property(reachedB, H)), winRowCheck(T).

case1(Grid) :-
    property(mined, Grid), not(property(flagged, Grid)).
case2(Grid) :-
    not(property(mined, Grid)), property(flagged, Grid).

allMinesOnFlagRow([]).
allMinesOnFlagRow([H|T]) :-
    not(case1(H)), not(case2(H)), allMinesOnFlagRow(T).

allMinesOnFlag([]).
allMinesOnFlag([H|T]) :-
    allMinesOnFlagRow(H), allMinesOnFlag(T).

neg(true, false).
neg(false, true).


modeInput(Mode) :-
    read(Mode),
    (Mode == 1; Mode == 0).

% getValidInput(X, Lower, Upper): Get an input X from user, and test if it is in [Lower, Upper]
getValidInput(X, Lower, Upper) :-
    read(X), integer(X), X >= Lower, X =< Upper, !.
getValidInput(X, Lower, Upper) :-
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

diffFile(0, "SuperEasy.txt").
diffFile(1, "Easy.txt").
diffFile(2, "Medium.txt").
diffFile(3, "Difficult.txt").

writeRecords(Name, Difficult, StartTime) :-
    get_time(EndTime),
    DiffTime is EndTime - StartTime,
    format(atom(TS), '~2f', DiffTime),
    write("Your clear time: "),
    write(TS), write('s'), nl,
    diffFile(Difficult, File),
    open(File, read, Str),
    read_string(Str, "", "\r", _, X), !,
    split_string(X, "\n", "", Lst),
    linesToRecords(Lst, Records),
    atom_number(TS, TN),
    append([(TN, Name)], Records, NRecords),
    sort(NRecords, SNRecords),
    mytake(5, SNRecords, NNRecords),
    close(Str), !,
    open(File, write, OS),
    recordsToLines(NNRecords, Lines),
    write(OS, Lines),
    close(OS), !.

recordsToLines([], "").
recordsToLines([(Time, Name)|T], Str) :-
    number_string(Time, TimeS),
    string_concat(Name, " ", S),
    string_concat(S, TimeS, SS),
    string_concat(SS, "\n", SSS),
    recordsToLines(T, SSSS),
    string_concat(SSS, SSSS, Str).

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
    not(dif(Lst, [""])),
    append([], NRecords, Records),
    linesToRecords(T, NRecords), !.
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

% checkLoseGame([], _, _, _).
checkLoseGame([H|T], C, Row, Column, NN):-
    C < Row, 
    NC is C + 1,
    (checkLoseRow(H, 0, Column, N) -> NN is N;
    otherwise -> checkLoseGame(T, NC, Row, Column, NN)).

% checkLoseRow([], _, _). Grid(location, mined, reached, reachedA, reachedB, flagged, flaggedA, flaggedB, num)

checkLoseRow([H|T], C, Column, NN):-
    C < Column,
    NC is C + 1,
    (reachedMine(H, N) -> NN is N;
    otherwise -> checkLoseRow(T, NC, Column, NN)).

reachedMine(Grid, 0):-
    property(reached, Grid),
    property(mined, Grid).

reachedMine(Grid, 1):-
    property(reachedA, Grid),
    property(mined, Grid).

reachedMine(Grid, 2):-
    property(reachedB, Grid),
    property(mined, Grid).

generateRanking(DifficultS) :-
    string_concat(DifficultS, ".txt", File),
    open(File, read, Str),
    read_string(Str, "", "\r", _, X),
    rankingString(DifficultS, X),
    close(Str), !.

rankingString(DifficultS, "") :-
    difficultFileAndTitle(DifficultS, TitleS),
    string_concat("  ", TitleS, Title),
    write(Title), nl,
    write("----------------"), nl,
    write(""), nl, !.

rankingString(DifficultS, X) :-
    dif(X, ""),
    split_string(X, "\n", "", Lst),
    linesToRecords(Lst, Records),
    sort(Records, SortRecords),
    difficultFileAndTitle(DifficultS, TitleS),
    string_concat("  ", TitleS, Title),
    write(Title), nl,
    write("----------------"), nl,
    createRank(SortRecords, 1, S),
    write(S), nl, !.
