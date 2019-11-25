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
    string_concat("[X]", NS, S).
generateRow([grid(_, _, _, _, _, true, _, _, _)|T], S) :-
    generateRow(T, NS),
    string_concat("[F]", NS, S).
generateRow([grid(_, _, true, true, _, _, _, _, 0)|T], S) :-
    generateRow(T, NS),
    string_concat("[A]", NS, S).
generateRow([grid(_, _, true, _, true, _, _, _, 0)|T], S) :-
    generateRow(T, NS),
    string_concat("[A]", NS, S).
generateRow([grid(_, _, true, _, _, _, _, _, Num)|T], S) :-
    Num \= 0, generateRow(T, NS),
    number_string(Num, NumS),
    string_concat("[", NumS, NumSS),
    string_concat(NumSS, "]", NumSSS),
    string_concat(NumSSS, NS, S).
generateRow([grid(_, false, _, _, _, _, _, _, _)|T], S) :-
    generateRow(T, NS),
    string_concat("[|]", NS, S).

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
    length(H, L), Col < L, L < 10, Col \= 0,
    number_string(Col, ColS),
    string_concat("  ", ColS, SS),
    NCol is Col + 1,
    generateColumnCoord([H|T], NCol, NS),
    string_concat(SS, NS, S).
generateColumnCoord([H|T], Col, S) :-
    length(H, L), Col < L, L >= 10,
    number_string(Col, ColS),
    string_concat(" ", ColS, SS),
    NCol is Col + 1,
    generateColumnCoord([H|T], NCol, NS),
    string_concat(SS, NS, S).
generateColumnCoord([H|_], Col, "\n") :-
    length(H, L), L = Col.

printTest :-
    minesPositions(4,4,4, P),
    buildBoard(0,0,4, 4, P, G),
    generateColumnCoord(G, 0, Cs),
    generateBoard(G, 0, S),
    write(Cs),
    write(S).
