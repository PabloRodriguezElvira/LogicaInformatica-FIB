%Problema A:
%Adapta el problema de “cifras” de la página 15 de la hoja p6 de los apuntes para que
%también use la división (sin dar nunca división por cero) y además encuentre las soluciones más cortas
%primero (las que menos elementos de la lista L usan).

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

pert_con_resto(X,L,Resto):- concat(L1,[X|L2],L), concat(L1,L2,Resto).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).

length_of_list([], 0).
length_of_list([_|List], Count):-
    length_of_list(List, Count1), Count is Count1+1.

%subsetOfSizeK(K, Set, Subset) -> Subset es un subconjunto de Set con cardinalidad k.
subsetOfSizeK(0, _, []):- !.
subsetOfSizeK(K, [X|Set], [X|Subset]):- K1 is K-1, subsetOfSizeK(K1, Set, Subset).
subsetOfSizeK(K, [_|Set], Subset):- subsetOfSizeK(K, Set, Subset).
%--------------------------------------------------------------------------------------------------------------------%

% PREDICADO CIFRAS
cifras(ListaNumeros, Resultado):-
    length_of_list(ListaNumeros, Length), between(1, Length, K), subsetOfSizeK(K, ListaNumeros, Subconjunto), 
    permutacion(Subconjunto, Permutacion), expresion(Permutacion, Expresion), 
    Resultado is Expresion, write(Expresion), nl, fail.                       

expresion([X],X).                                             
expresion(L, E1/E2):- concat(L1, L2, L), L1 \= [], L2 \= [],
                    expresion(L1, E1), expresion(L2, E2),
                    X is E1, Y is E2, Y \= 0, 0 is X mod Y. 

expresion(L,E1+E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      

expresion(L,E1-E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      

expresion(L,E1*E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).  


%Problema B:
%Adapta (es obligatorio) el esquema Prolog de abajo para resolver los tres problemas B1,B2,B3. Lo
%único que hay que hacer en cada uno de ellos es; a) definir qué es un estado y b) qué pasos hay para
%pasar de un estado al siguiente.


%Problema B.1:
%Buscamos la manera más rápida para tres misioneros y tres canı́bales de cruzar un rı́o,
%disponiendo de una canoa que puede ser utilizada por 1 ó 2 personas (misioneros o canı́bales), pero
%siempre evitando que los misioneros queden en minorı́a en cualquier orilla (por razones obvias).

mainB1:- 
    % OX-[numMisioneros1-numCanibales1]-[numMisioneros2-numCanibales2].
    % 1 -> orilla 1, 2 -> orilla2. oX, donde X es {1,2} indica que estamos en la orilla 1 o 2.
    EstadoInicial = o1-[3-3]-[0-0],
    EstadoFinal = o2-[0-0]-[3-3], 
    between(1, 1000, CosteMax),
    % Buscamos solución de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), write('con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
    CosteMax > 0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
    \+ member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

cruzar([M1-C1]-[M2-C2], [NewM1-NewC1]-[NewM2-NewC2]):-
    between(0, 2, M), between(0, 2, C), %enviar entre 0 y 2 misioneros y canibales.
    Total is M+C, Total > 0, Total =< 2,
    NewM1 is M1-M, NewM1 >= 0, NewC1 is C1-C, NewC1 >= 0,
    NewM1 >= NewC1, %los misioneros no están en minoria
    NewM2 is M2+M, NewC2 is C2+C,
    NewM2 >= NewC2. %los misioneros no están en minoria.

%De la orilla 1 a la orilla 2.
unPaso(1, o1-[M1-C1]-[M2-C2], o2-[M1next-C1next]-[M2next-C2next]):-
    cruzar([M1-C1]-[M2-C2], [M1next-C1next]-[M2next-C2next]).

%De la orilla 2 a la orilla 1.
unPaso(1, o2-[M1-C1]-[M2-C2], o1-[M1next-C1next]-[M2next-C2next]):-
    cruzar([M1-C1]-[M2-C2], [M1next-C1next]-[M2next-C2next]).


% % B.2. Dado un natural n > 0, una posición inicial (FilaI , ColumnaI ), una posición final (FilaF , ColumnaF ),
% % y un número de pasos P , encontrar un camino de (FilaI , ColumnaI ) a (FilaF , ColumnaF ), en un
% % tablero de ajedrez de n × n en exactamente P pasos de caballo. El programa ha de fallar si para la n
% % en cuestión no existe tal camino.

mainB2:- 
    EstadoInicial = [0-0], %posicion [X-Y] en el tablero.
    EstadoFinal = [6-3], %posicion [X-Y] en el tablero.
    N = 8, P = 3,
    camino(N, P, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), nl, halt.

%N -> dimension del tablero, P -> numero de pasos
camino(_, 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.
camino(N, P, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
    unPaso(EstadoActual, EstadoSiguiente),
    esCorrecto(N, EstadoSiguiente),
    P1 is P-1,
    P1 >= 0,
    \+ member( EstadoSiguiente, CaminoHastaAhora),
    camino(N, P1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

esCorrecto(N, [X-Y]):-
    X >= 0, X < N,
    Y >= 0, Y < N.

unPaso([X-Y], [NewX-NewY]):-
    NewX is X+1,
    NewY is Y+2.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X-1,
    NewY is Y+2.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X+2,
    NewY is Y+1.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X-2,
    NewY is Y+1.

unPaso([X-Y], [NewX-NewY]):-
    NewX is X+1,
    NewY is Y-2.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X+2,
    NewY is Y-1.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X-1,
    NewY is Y-2.
unPaso([X-Y], [NewX-NewY]):-
    NewX is X-2,
    NewY is Y-1.

% B.3. Trata de averiguar la manera más rápida que tienen cuatro personas P1, P2, P5 y P8 para
% cruzar de noche un puente que sólo aguanta el peso de dos, donde tienen una única e imprescindible
% linterna y cada Pi tarda i minutos en cruzar. Dos juntos tardan como el más lento de los dos.

mainB3:-
    %estado: [P1, P2, P5, P8]. 0 significa que la persona se encuentra en el lado inicial, 1 es que está en el lado final.
    %l1 significa que la linterna está en el lado 1 (inicio). l2 es que la linterna está en el lado2 (final).
    EstadoInicial = [0,0,0,0]-l1, 
    EstadoFinal = [1,1,1,1]-l2, 
    between(1, 1000, CosteMax),
    % Buscamos solución de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), write('con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ) :-
    CosteMax > 0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),
    \+ member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino(CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

%Con coste 1 solo se puede mover P1.
unPaso(1, [0, P2, P5, P8]-l1, [1, P2, P5, P8]-l2).
unPaso(1, [1, P2, P5, P8]-l2, [0, P2, P5, P8]-l1).

%Con coste 2 se pueden mover P1 y P2, o solo P2.
unPaso(2, [0, 0, P5, P8]-l1, [1, 1, P5, P8]-l2).
unPaso(2, [1, 1, P5, P8]-l2, [0, 0, P5, P8]-l1).
unPaso(2, [P1, 0, P5, P8]-l1, [P1, 1, P5, P8]-l2).
unPaso(2, [P1, 1, P5, P8]-l2, [P1, 0, P5, P8]-l1).

%Con coste 5 se pueden mover P1 y P5, o P2 y P5, o solo P5.
unPaso(5, [0, P2, 0, P8]-l1, [1, P2, 1, P8]-l2).
unPaso(5, [1, P2, 1, P8]-l2, [0, P2, 0, P8]-l1).
unPaso(5, [P1, 0, 0, P8]-l1, [P1, 1, 1, P8]-l2).
unPaso(5, [P1, 1, 1, P8]-l2, [P1, 0, 0, P8]-l1).
unPaso(5, [P1, P2, 0, P8]-l1, [P1, P2, 1, P8]-l2).
unPaso(5, [P1, P2, 1, P8]-l2, [P1, P2, 0, P8]-l1).

%Con coste 8 se puede mover P1 y P8, o P2 y P8, o P5 y P8, o solo P8. 
unPaso(8, [0, P2, P5, 0]-l1, [1, P2, P5, 1]-l2).
unPaso(8, [1, P2, P5, 1]-l2, [0, P2, P5, 0]-l1).
unPaso(8, [P1, 0, P5, 0]-l1, [P1, 1, P5, 1]-l2).
unPaso(8, [P1, 1, P5, 1]-l2, [P1, 0, P5, 0]-l1).
unPaso(8, [P1, P2, 0, 0]-l1, [P1, P2, 1, 1]-l2).
unPaso(8, [P1, P2, 1, 1]-l2, [P1, P2, 0, 0]-l1).
unPaso(8, [P1, P2, P5, 0]-l1, [P1, P2, P5, 1]-l2).
unPaso(8, [P1, P2, P5, 1]-l2, [P1, P2, P5, 0]-l1).