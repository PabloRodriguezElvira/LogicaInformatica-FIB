:-use_module(library(clpfd)).

% Complete the following program p(N) that writes a (kind of) magic
% square: an NxN matrix with ALL the numbers 1..N^2, such that the
% sum of every row and every column is equal.
% More precisely, this sum is (N + N^3) / 2.
% Note: don't worry if your program is (too) slow when N >= 6.

%% Example:  (this solution is not unique):
%%
%%    1   2  13  24  25
%%    3   8  18  17  19
%%   16  14  15   9  11
%%   22  20   7  10   6
%%   23  21  12   5   4

main:- p(6), nl, halt.

%1. Domain, 2. Constraints, 3. Labelling, 4. Write.
p(N):-
    NSquare is N*N,
    length( Vars, NSquare ),

    %1. Domain:
    Vars ins 1..NSquare,
    all_distinct(Vars),

    %2. Constraints:
    squareByRows(N,Vars,SquareByRows),
    transpose( SquareByRows, SquareByCols ),  % transpose already exists: no need to implement it
    Sum is (N + N*N*N) // 2,
    constraintsSum(Sum, SquareByRows),
    constraintsSum(Sum, SquareByCols), 

    %3. Labelling:
    labeling([ff], Vars),

    %4. Write:
    writeSquare(SquareByRows),nl,!.


squareByRows(_,[],[]):-!.
squareByRows(N,Vars,[Row|SquareByRows]):- append(Row,Vars1,Vars), length(Row,N), squareByRows(N,Vars1,SquareByRows),!.

writeSquare(Square):- member(Row,Square), nl, member(N,Row), write4(N), fail.
writeSquare(_).

write4(N):- N<10,   write('   '), write(N),!.
write4(N):- N<100,  write('  ' ), write(N),!.
write4(N):-         write(' '  ), write(N),!.

constraintsSum(_, []).
constraintsSum(Sum, [Elem|Square]):-
    sum(Elem, #=, Sum),
    constraintsSum(Sum, Square).


    