%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]

%%% generateSubsets(L, S1, S2) -> S1 U S2 = L.
generate_disjoint_subsets([], [], []).
generate_disjoint_subsets([X|L], [X|S1], S2):- generate_disjoint_subsets(L, S1, S2).
generate_disjoint_subsets([X|L], S1, [X|S2]):- generate_disjoint_subsets(L, S1, S2).

sum([], 0).
sum([First|List], N):- sum(List, N1), N is N1+First.

eqSplit(L,S1,S2):- 
    generate_disjoint_subsets(L, S1, S2),
    sum(S1, N1),
    sum(S2, N2),
    N1 = N2,
    write(S1), write('    '), write(S2), nl, fail.





