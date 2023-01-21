padre(juan, pedro).
padre(maria, pedro).
padre(raul, sixtynineeee).
hermano(pedro, vicente).
hermano(pedro, alberto).

tio(S, T):- padre(S, P), hermano(P, T).
%Hacer ejercicio cifras pizarra labo.