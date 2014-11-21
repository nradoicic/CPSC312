/* Nikola Radoicic
*  30478093
*  z9k7
*
*  This should run test, but not much else. */
test :-
    test0, nl,
    test0a, nl,
    test0b, nl,
    test0c.

/* This is a completly solved solution. */
test0 :-
    L = [
             [9,6,3,1,7,4,2,5,8],
             [1,7,8,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,9,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,5,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) which
should be found very quickly. */
test0a :-
    L = [
             [9,_,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This has a solution (the one in test0) and
may take a few seconds to find. */
test0b :-
    L = [
             [9,_,3,1,7,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* This one obviously has no solution (column 2 has
two nines in it.) and it may take a few seconds
to deduce this. */
test0c :-
    L = [
             [_,9,3,1,7,4,2,5,8],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,8,9,7,3,1],
             [8,2,1,4,3,7,5,_,6],
             [4,9,6,8,5,2,3,1,7],
             [7,3,_,9,6,1,8,2,4],
             [5,8,9,7,1,3,4,6,2],
             [3,1,7,2,4,6,9,8,5],
             [6,4,2,5,9,8,1,7,3]],
        sudoku(L),
        printsudoku(L).

/* Here is an extra test for you to try. It would be
nice if your program can solve this puzzle, but it's
not a requirement. */

test0d :-
    L = [
             [9,_,3,1,_,4,2,5,_],
             [_,7,_,3,2,5,6,4,9],
             [2,5,4,6,_,9,_,3,1],
             [_,2,1,4,3,_,5,_,6],
             [4,9,_,8,_,2,3,1,_],
             [_,3,_,9,6,_,8,2,_],
             [5,8,9,7,1,3,4,6,2],
             [_,1,7,2,_,6,_,8,5],
             [6,4,2,5,_,8,1,7,3]],
        sudoku(L),
        printsudoku(L).


/* The next 3 tests are supposed to be progressively
harder to solve. Our first attempt at a solver did not
find a solution in a reasonable length of time for
any of these, so if you manage to write a solver
that does them in a reasonable length of time,
expect to recieve top or possibly bonus marks. (BUT
YOU MUST TELL US THIS IN YOUR SUBMISSION OR WE WON'T
RUN THESE TESTS.) */
test1 :-
    L = [
             [_,6,_,1,_,4,_,5,_],
             [_,_,8,3,_,5,6,_,_],
             [2,_,_,_,_,_,_,_,1],
             [8,_,_,4,_,7,_,_,6],
             [_,_,6,_,_,_,3,_,_],
             [7,_,_,9,_,1,_,_,4],
             [5,_,_,_,_,_,_,_,2],
             [_,_,7,2,_,6,9,_,_],
             [_,4,_,5,_,8,_,7,_]],
        sudoku(L),
        printsudoku(L).

test2 :-
    L = [
             [_,_,4,_,_,3,_,7,_],
             [_,8,_,_,7,_,_,_,_],
             [_,7,_,_,_,8,2,_,5],
             [4,_,_,_,_,_,3,1,_],
             [9,_,_,_,_,_,_,_,8],
             [_,1,5,_,_,_,_,_,4],
             [1,_,6,9,_,_,_,3,_],
             [_,_,_,_,2,_,_,6,_],
             [_,2,_,4,_,_,5,_,_]],
        sudoku(L),
        printsudoku(L).

test3 :-
    L = [
             [_,4,3,_,8,_,2,5,_],
             [6,_,_,_,_,_,_,_,_],
             [_,_,_,_,_,1,_,9,4],
             [9,_,_,_,_,4,_,7,_],
             [_,_,_,6,_,8,_,_,_],
             [_,1,_,2,_,_,_,_,3],
             [8,2,_,5,_,_,_,_,_],
             [_,_,_,_,_,_,_,_,5],
             [_,3,4,_,9,_,7,1,_]],
             %        sudoku(L),
        printsudoku(L).


% print suduko table
printsudoku([]).
printsudoku([H|T]) :-
    write(H),nl,
    printsudoku(T).


% Expects a list of lists 9 by 9 grid.
sudoku(L) :- sudoku(L,_).

% YOU NEED TO COMPLETE THIS PREDICATE, PLUS PROVIDE ANY HELPER PREDICATES BELOW.

/* Regex wrote my HW for me...
 * The idea is to create a list for each sequence of
 * variables on the board which need to be constrained
 * by the fact that it needs to be a permutation from
 * 1-9.
 *
 * Why write out all the things explicitly?
 * Fewer inferences and more readability. */
sudoku(Board, Solution) :-
    Solution = Board,
    Board = [[A1,B1,C1,D1,E1,F1,G1,H1,I1],
             [A2,B2,C2,D2,E2,F2,G2,H2,I2],
             [A3,B3,C3,D3,E3,F3,G3,H3,I3],
             [A4,B4,C4,D4,E4,F4,G4,H4,I4],
             [A5,B5,C5,D5,E5,F5,G5,H5,I5],
             [A6,B6,C6,D6,E6,F6,G6,H6,I6],
             [A7,B7,C7,D7,E7,F7,G7,H7,I7],
             [A8,B8,C8,D8,E8,F8,G8,H8,I8],
             [A9,B9,C9,D9,E9,F9,G9,H9,I9]],
    % Rows
    % These are the unique rows on the board
    Row_1 = [A1,B1,C1,D1,E1,F1,G1,H1,I1],
    Row_2 = [A2,B2,C2,D2,E2,F2,G2,H2,I2],
    Row_3 = [A3,B3,C3,D3,E3,F3,G3,H3,I3],
    Row_4 = [A4,B4,C4,D4,E4,F4,G4,H4,I4],
    Row_5 = [A5,B5,C5,D5,E5,F5,G5,H5,I5],
    Row_6 = [A6,B6,C6,D6,E6,F6,G6,H6,I6],
    Row_7 = [A7,B7,C7,D7,E7,F7,G7,H7,I7],
    Row_8 = [A8,B8,C8,D8,E8,F8,G8,H8,I8],
    Row_9 = [A9,B9,C9,D9,E9,F9,G9,H9,I9],

    % Columns
    % These are the unique columns on the board
    Col_1 = [A1,A2,A3,A4,A5,A6,A7,A8,A9],
    Col_2 = [B1,B2,B3,B4,B5,B6,B7,B8,B9],
    Col_3 = [C1,C2,C3,C4,C5,C6,C7,C8,C9],
    Col_4 = [D1,D2,D3,D4,D5,D6,D7,D8,D9],
    Col_5 = [E1,E2,E3,E4,E5,E6,E7,E8,E9],
    Col_6 = [F1,F2,F3,F4,F5,F6,F7,F8,F9],
    Col_7 = [G1,G2,G3,G4,G5,G6,G7,G8,G9],
    Col_8 = [H1,H2,H3,H4,H5,H6,H7,H8,H9],
    Col_9 = [I1,I2,I3,I4,I5,I6,I7,I8,I9],

    % Blocks
    % These are the blocks (or squares) of the board
    Blk_1 = [A1,A2,A3,B1,B2,B3,C1,C2,C3],
    Blk_2 = [D1,D2,D3,E1,E2,E3,F1,F2,F3],
    Blk_3 = [G1,G2,G3,H1,H2,H3,I1,I2,I3],
    Blk_4 = [A4,A5,A6,B4,B5,B6,C4,C5,C6],
    Blk_5 = [D4,D5,D6,E4,E5,E6,F4,F5,F6],
    Blk_6 = [G4,G5,G6,H4,H5,H6,I4,I5,I6],
    Blk_7 = [A7,A8,A9,B7,B8,B9,C7,C8,C9],
    Blk_8 = [D7,D8,D9,E7,E8,E9,F7,F8,F9],
    Blk_9 = [G7,G8,G9,H7,H8,H9,I7,I8,I9],

    % A list of all the sequences which need need to have the numbers 1-9
    All   = [Row_1,Row_2,Row_3,Row_4,Row_5,Row_6,Row_7,Row_8,Row_9,
             Col_1,Col_2,Col_3,Col_4,Col_5,Col_6,Col_7,Col_8,Col_9,
             Blk_1,Blk_2,Blk_3,Blk_4,Blk_5,Blk_6,Blk_7,Blk_8,Blk_9],

    % Ensure that each is a permutation of numbers 1-9
    maplist(permutation([1,2,3,4,5,6,7,8,9]), All).
