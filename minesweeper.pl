firTup((X,_), X).       % First element in the tuple
secTup((_,X), X).       % Second element in the tuple

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
printTest :-
    buildBoard(0,0,4, 4,[(0,0), (1,1),(2,2),(3,3)], G),
    generateBoard(G, 0, S),
    write(S).
