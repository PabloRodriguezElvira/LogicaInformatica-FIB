%Minimo en una lista
min_in_list([Min],Min).                 

min_in_list([E1,E2|Tail], Min):- E1 =< E2, min_in_list([E1|Tail], Min), !. %E1 <= E2, descartamos E2.                          

min_in_list([E1,E2|Tail], Min):- E1 > E2, min_in_list([E2|Tail], Min), !. %E1 > E2, descartamos E1.
                                 
%L1 y L2 contienen los mismos elementos.
sameEl([], _).
sameEl([E1|Tail1], List2):- member(E1, List2), !, sameEl(Tail1, List2).