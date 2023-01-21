% A latin square is an NxN matrix where each row and each column contains all numbers from 1 to N.
% (Note that a sudoku is a 9x9 latin square with additional constraints on the 3x3 boxes).
% Given a partially filled matrix, we want to find ALL its possible completions to make a latin square.
% Complete the following code to do that:

:- use_module(library(clpfd)). 

main:- example2.

example1:- latin([2,_,_,3,        %this example has only one solution:  2 4 1 3
                  3,_,_,_,                                          %   3 1 2 4
                  _,_,_,1,                                          %   4 2 3 1
                  _,_,4,_]).                                        %   1 3 4 2


example2:- latin([ 5,3,_,_,7,_,8,1,2,   %this one has 2 solutions
                   6,_,_,1,9,5,_,_,_,
                   _,9,8,_,_,_,1,6,7,
                   8,_,_,_,6,_,9,_,3,
                   4,_,_,8,_,3,_,_,1,
                   7,_,_,_,2,_,3,_,6,
                   _,6,_,_,_,_,2,8,_,
                   _,_,_,4,1,9,7,_,5,
                   _,4,_,_,8,_,_,7,9]).

latin(L):-
    length(L,Len), S is round(sqrt(Len)),  Len is S*S, %% sqrt means square root.  Len must be a perfect square
    %1. Dominio:
    L ins 1..S,
    %2. Constraints:
    matrixByRows(S, L, Rows),     
    constraintsFromSubLists(Rows), 
    write('hola'),
    transpose(Rows,Cols),           % the transpose predicate already exists in SWI prolog, no need to implement it
    constraintsFromSubLists(Cols),
    %3. Labelling:
    label(L),
    %4. Write:
    nl,nl,displaySol(Rows), nl,nl, fail.
latin(_).

displaySol(Rows):- member(Row,Rows), nl, member(N,Row), write(N), write(' '), fail.
displaySol(_).

matrixByRows(_, [], []).
matrixByRows(LengthRow, Mat, [Row|MatrixByRows]):-
    length(Row, LengthRow),
    append(Row, RestMat, Mat),
    matrixByRows(LengthRow, RestMat, MatrixByRows).

constraintsFromSubLists([]).
constraintsFromSubLists([Elem|Matrix]):-
    all_distinct(Elem),
    constraintsFromSubLists(Matrix).
