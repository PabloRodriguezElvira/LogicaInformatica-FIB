main:-
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),
    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,
    append(D1, D2, D12),
    append(D12, D3, D123),
    append(D123, D4, Vars),
    findall([N1, N2, N3, N4], (word([A, B, C, D]), num(A, N1), num(B, N2), num(C, N3), num(D, N4)), Words),
    allWordsDifferentDices(D1, D2, D3, D4, Words),
    all_distinct(Vars),
    label(Vars),
    writeN(D1), 
    writeN(D2), 
    writeN(D3), 
    writeN(D4), halt.
    
writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.

allWordsDifferentDices(_, _, _, _, []).
allWordsDifferentDices(D1, D2, D3, D4, [Word|Words]) :-
    differentDices(D1, D2, D3, D4, Word),
    allWordsDifferentDices(D1, D2, D3, D4, Words)
.

differentDices(D1, D2, D3, D4, [A, B, C, D]) :- 
    letterInDice(D1, [A,B,C,D]), letterInDice(D1, [B,A,C,D]), letterInDice(D1, [C,B,A,D]), letterInDice(D1, [D,B,C,A]),
    letterInDice(D2, [A,B,C,D]), letterInDice(D2, [B,A,C,D]), letterInDice(D2, [C,B,A,D]), letterInDice(D2, [D,B,C,A]),
    letterInDice(D3, [A,B,C,D]), letterInDice(D3, [B,A,C,D]), letterInDice(D3, [C,B,A,D]), letterInDice(D3, [D,B,C,A]),
    letterInDice(D4, [A,B,C,D]), letterInDice(D4, [B,A,C,D]), letterInDice(D4, [C,B,A,D]), letterInDice(D4, [D,B,C,A]).


letterInDice([A, B, C, D, E, F], [Letter1, Letter2, Letter3, Letter4]) :-
    ((A #= Letter1 #\/
    B #= Letter1 #\/
    C #= Letter1 #\/
    D #= Letter1 #\/
    E #= Letter1 #\/
    F #= Letter1) #==>
    

    ((A #\= Letter2 #/\
    B #\= Letter2 #/\
    C #\= Letter2 #/\
    D #\= Letter2 #/\
    E #\= Letter2 #/\
    F #\= Letter2) 
    #/\

    (A #\= Letter3 #/\
    B #\= Letter3 #/\
    C #\= Letter3 #/\
    D #\= Letter3 #/\
    E #\= Letter3 #/\
    F #\= Letter3) 
    #/\

    (A #\= Letter4 #/\
    B #\= Letter4 #/\
    C #\= Letter4 #/\
    D #\= Letter4 #/\
    E #\= Letter4 #/\
    F #\= Letter4)))
.