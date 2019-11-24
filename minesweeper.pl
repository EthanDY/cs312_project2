firTup((X,_), X).       % First element in the tuple
secTup((_,X), X).       % Second element in the tuple


% Grid(location, mined, reachedA, reachedB, flagged, num)
% initRow(row, column, length, [mines_locations], [grids_of_this_row])
grid(_, _, _, _, _, _).
initRow(_, Y, Y, _, []).
initRow(X, Y, L, Z, R) :-
    Y<L, member((X,Y), Z), append([grid((X,Y), true, false, false, false, 0)], NR, R),
    NY is Y+1,
    initRow(X, NY, L, Z, NR).
initRow(X, Y, L, Z, R) :-
    Y<L, not(member((X,Y), Z)), append([grid((X,Y), false, false, false, false, 0)], NR, R),
    NY is Y+1,
    initRow(X, NY, L, Z, NR).