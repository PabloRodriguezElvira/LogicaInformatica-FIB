%1. Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista
%de enteros dada L”. Debe poder generar la P y también comprobar una P dada.

prod([], 1).
prod([X|Lista], P):- prod(Lista, P1), P is X*P1.


%2. Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los
%vectores L1 y L2”, donde los vectores se representan como listas de enteros. El predicado debe
%fallar si los dos vectores tienen longitudes distintas.

pescalar([], [], 0).
pescalar([X1|Lista1], [X2|Lista2], P):- pescalar(Lista1, Lista2, P1), P is P1 + X1*X2.


%3. Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de
%intersección y unión de conjuntos dados. 

%intersec(L1, L2, L3) -> L3 es la intersección de L1 y L2.
intersec([], _, []).
intersec([X|L1], L2, [X|L3]):- miembroLista(X, L2), !, intersec(L1, L2, L3).
intersec([_|L1], L2, L3):- intersec(L1, L2, L3).


%union(L1, L2, L3) -> L3 es la unión de L1 y L2.
mi_union([], L2, L2).
mi_union([X|L1], L2, L3):- miembroLista(X, L2), !, mi_union(L1, L2, L3).
mi_union([X|L1], L2, [X|L3]):- mi_union(L1, L2, L3).

miembroLista(X, [X|_]).
miembroLista(X, [_|L]):- miembroLista(X, L).


%4.Usando append, escribe un predicado para calcular el último elemento de una lista dada, y otro
%para calcular la lista inversa de una lista dada.

%lastElement(L, X) -> X es el último elemento de la lista L.
lastElement(L, X):- append(_, [X], L).

%reverseList(L, RL) -> RL es la lista inversa de L.
reverseList([], []).
reverseList(L, [X|RL]):- lastElement(L, X), removeLast(L, L2), reverseList(L2, RL).  

removeLast(L, Res):- append(Res, [_], L).

%5.Escribe un predicado fib(N,F) que signifique: “F es el N-ésimo número de Fibonacci para la
%N dada”. Estos números se definen ası́: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N ) =
%fib(N − 1) + fib(N − 2).

fib(0, 0):- !.
fib(1, 1):- !.
fib(N, F):- N1 is N-1, N2 is N-2, fib(N1, F1), fib(N2, F2), F is F1 + F2.


%6. Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar
%P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una solución serı́a [1,4] (nótese
%que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de
%generar todas las soluciones posibles.
% P = 1, N = 1 -> [1].
% P = 4, N = 2 -> [1,3],[2,2].

dados(0, 0, []).
dados(P, N, [X|L]):- N > 0, miembroLista(X, [1,2,3,4,5,6]), N1 is N-1, P1 is P-X, dados(P1, N1, L).


%7. Escribe un predicado suma_demas(L) que, dada una lista de enteros L, se satisface si existe algún
%elemento en L que es igual a la suma de los demás elementos de L, y falla en caso contrario.
%L = [1,2,3] --> SI porque 3 = 2+1.
%L = [1,2] --> NO.

suma_demas(L):- append(L1, [X|L2], L), append(L1, L2, L3), suma(L3, X).

suma([], 0):- !.  
suma([X|L], S):- suma(L, S1), S is S1+X.

%8. Escribe un predicado suma_ants(L) que, dada una lista de enteros L, se satisface si existe algún
%elemento en L que es igual a la suma de los elementos anteriores a él en L, y falla en caso
%contrario.

suma_ants(L):- append(L1, [X|_], L), suma(L1, X).

%9. Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
%elemento de L, dice cuántas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
%card( [1,2,1,5,1,3,3,7] ) el intérprete escribirá:
% [[1,3],[2,1],[5,1],[3,2],[7,1]]. 

card(L):- card_rec(L, Res), write(Res).

card_rec([], []).
card_rec([Numero|Lista], [[Numero, Apariciones]|Res]):- 
    card_rec(Lista, ResTotal), 
    quitar_elem([Numero, AparicionesAnt], ResTotal, Res), !, 
    Apariciones is AparicionesAnt+1.
card_rec([Numero|Lista], [[Numero, 1]|Res]):- card_rec(Lista, Res).

%NewL = L - [X].
quitar_elem(X, L, NewL):- append(L1, [X|L2], L), append(L1, L2, NewL).


%10. Escribe un predicado esta ordenada(L) que signifique: “la lista L de números enteros está
%ordenada de menor a mayor”. Por ejemplo, a la consulta:
%?-esta ordenada([3,45,67,83]).
%el intérprete responde yes, y a la consulta:
%?-esta ordenada([3,67,45]).
%responde no.

ordenada([]).
ordenada([_]).
ordenada([X1, X2|L]):- X1 =< X2, !, ordenada([X2|L]).


%11. Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de
%menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 será [2,3,3,4,5]. Hazlo en
%una lı́nea, usando sólo los predicados permutacion y esta ordenada.

ord([], []).
ord(L, Lord):- permutation(L, Lord), ordenada(Lord).


%12. Escribe un predicado diccionario(A,N) que, dado un alfabeto A de sı́mbolos y un natural N,
%escriba todas las palabras de N sı́mbolos, por orden alfabético (el orden alfabético es según el
%alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribirá:
%gaga gachu gale chuga chuchu chule lega lechu lele

%nSubsets(A, N, S): S = Un subconjunto de A con cardinalidad N.
nsubsets(_, 0, []):- !.
nsubsets(Alphabet, N, [Word|Subset]):-
    member(Word, Alphabet),
    N1 is N-1,
    nsubsets(Alphabet, N1, Subset).


writePalabra([]):- write(" "), !.
writePalabra([Elem|Lista]):-
    write(Elem),
    writePalabra(Lista).


diccionario(Alphabet, N):-
    nsubsets(Alphabet, N, Sub),
    writePalabra(Sub), fail.


%13. Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las per-
%mutaciones de sus elementos que sean palı́ndromos (capicúas). Por ejemplo, con la consulta
%palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].


es_palindromo([]).
es_palindromo([Elem|List]):-
    lastElement(List, X), 
    X = Elem, 
    removeLast(List, NewList),
    es_palindromo(NewList).


palindromos(Alphabet):-
    setof(Perm, (permutation(Alphabet, Perm), es_palindromo(Perm)), Res),
    write(Res).


%14. Encuentra mediante un programa Prolog, usando el predicado permutación, qué 8 dı́gitos difer-
%entes tenemos que asignar a las letras S, E, N, D, M, O, R, Y, de manera que se cumpla la suma
%siguiente:
%   S E N D
% + M O R E
% M O N E Y

sendMoreMoney:- 
    Letters = [S,E,N,D,M,O,R,Y,_,_],
    Numbers = [0,1,2,3,4,5,6,7,8,9],
    permutation(Letters, Numbers),

    First is S*1000 + E*100 + N*10 + D,
    Second is M*1000 + O*100 + R*10 + E,
    Suma is First + Second, 
    Suma is M*10000 + O*1000 + N*100 + E*10 + Y,
    writeValores(Letters), !.

%Escribimos los valores de cada letra.
writeValores([S,E,N,D,M,O,R,Y,_,_]):-
    write('S = '), write(S), nl,
    write('E = '), write(E), nl,
    write('N = '), write(N), nl,
    write('D = '), write(D), nl,
    write('M = '), write(M), nl,
    write('O = '), write(O), nl,
    write('R = '), write(R), nl,
    write('Y = '), write(Y), nl,
    writeSuma([S,E,N,D,M,O,R,Y]).

%Escirimos el resultado de la suma.
writeSuma([S,E,N,D,M,O,R,Y]):-
    First is S*1000 + E*100 + N*10 + D,
    Second is M*1000 + O*100 + R*10 + E,
    Suma is First + Second,
    Result is M*10000 + O*1000 + N*100 + E*10 + Y,

    write(' SEND = '), write(First), nl,
    write('+MORE = '), write(Second), nl,
    write('SEND+MORE = '), write(Suma), nl,
    write('------------'), nl,
    write('MONEY = '), write(Result).


%15. Escribe un predicado simplifica que pueda usarse en combinación con el programa de calcular
%derivadas.

% Programa del archivo pizarraLaboProlog.pl --> der( Expr, Var, Der )  == "la derivada de Expr con respecto Var es Der"
der( X, X, 1):- !.
der( C, _, 0):- atomic(C).% atomic significa que es una expresion constante o un entero
der( A+B, X, U+V ):- der(A,X,U), der(B,X,V). 
der( A*B, X, A*V+B*U ):- der(A,X,U), der(B,X,V). 

%Programa de p6sols.pl
simplifica(E,E1):- unpaso(E,E2),!, simplifica(E2,E1).
simplifica(E,E).

%Programa de p6sols.pl
unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.



% Queremos obtener en Prolog un predicado dom(L) que, dada una lista L de fichas de dominó (en
%el formato de abajo), escriba una cadena de dominó usando todas las fichas de L, o escriba “no
%hay cadena” si no es posible.

dom(L) :- p(L,P), ok(P), write(P), nl.
dom( ) :- write('no hay cadena'), nl.

%(a) ¿Qué significa el predicado p(L,P) para una lista L dada?
% Significa que la lista P es una permutación de la lista L.

%(b) Escribe el predicado ok(P) que falta.
ok([f(_,_)]).
ok([f(_, X2),f(Y1, Y2)|Lista]):-
    X2 is Y1, ok([f(Y1, Y2)|Lista]).

%(c) Extiende el predicado p para que el programa también pueda hacer cadenas girando alguna
%de las fichas de la entrada.

p([],[]).
p(L,[X|P]) :- select(X,L,R), p(R,P).
p(Lista, [f(X2, X1)|Perm]):- select(f(X1, X2), Lista, Resto), p(Resto, Perm).


%17. Complete the following backtracking procedure for SAT in Prolog. Program everything, except
%the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
%For example, p3 ∨ ¬p6 ∨ p2 is represented by [3,-6,2]. Do things as simple as possible.

%p:- readclauses(F), sat([],F).
p:- write('UNSAT'),nl.

sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I), nl, !.
sat(I,F):-
    decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
    simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
    sat(I, F1).

decision_lit([], []).
decision_lit([Clause|F], Lit):-
    unitClause(Clause), Lit is Clause,
    decision_lit(F, Lit).
decision_lit([Clause|F], Lit):-
    select(Lit, Clause, _),
    decision_lit(F, Lit).

unitClause(Clause):-
    numeroElementos(Clause, X), X is 1.

numeroElementos([], 0).
numeroElementos([_|Lista], N):-
   numeroElementos(Lista, N1), N is N1+1.

simplif(_, [], []).
simplif(Lit, [Clause|F], F1):-
    select(Lit, Clause, NewClause),
    simplif(Lit, F, [NewClause|F1]).


%18. Consider two groups of 10 people each. In the first group, as expected, the percentage of people
%with lung cancer among smokers is higher than among non-smokers. In the second group, the
%same is the case. But if we consider the 20 people of the two groups together, then the situation
%is the opposite: the proportion of people with lung cancer is higher among non-smokers than
%among smokers! Can this be true? Write a little Prolog program to find it out.

smokers:- 
    Numbers = [0,1,2,3,4,5,6,7,8,9],
    Group1 = [S1, SC1, NS1, NSC1,_,_,_,_,_,_],
    permutation(Group1, Numbers),
    10 = S1 + SC1 + NS1 + NSC1, SC1 > NSC1,
    Group2 = [S2, SC2, NS2, NSC2,_,_,_,_,_,_],
    permutation(Group2, Numbers),
    10 = S2 + SC2 + NS2 + NSC2, SC2 > NSC2,
    NSC is NSC1 + NSC2, NS is NS1 + NS2, SC is SC1 + SC2, S is S1 + S2,
    NSC/(NS+NSC) > SC/(S+SC).

%19. Supongamos que tenemos una máquina que dispone de monedas de valores [X1,...Xn] y tiene
%que devolver una cantidad C de cambio utilizando el mı́nimo número de monedas. Escribe un
%programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
%lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
%361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).
%Note: greedy algorithms (starting with the largest coin, etc.) do not always work!

maq(_, 0, _).
maq([Moneda|Lista], Cambio, [Cantidad|Monedas]):- 
    Cambio1 is Moneda*Cantidad, Cambio1 < Cambio, 
    Restante is Cambio - Cambio1, 
    maq(Lista, Restante, Monedas), fail.


%20. Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
%example:
%?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
%F=[a,b,c,d,e,f,g,h]?

flatten([], []):- !.
flatten(X, [X]):- X \= [_|_].
flatten([Lista1|Lista2], Flatten):- 
    flatten(Lista1, Flatten1), 
    flatten(Lista2, Flatten2), 
    append(Flatten1, Flatten2, Flatten).


%21. Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
%de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
%Podéis usar la exponenciación, como en 125 is 5**3. El programa (completo) no debe ocupar
%más de 3 lineas.

log(_, 1, 0).
log(Base, Arg, Result):-
    Arg > 1,
    Arg1 is Arg/Base,
    log(Base, Arg1, ResInd),
    Result is ResInd+1.


%22. Supongamos que N estudiantes (identificados por un número entre 1 y N) se quieren matricular
%de LI, pero sólo hay espacio para M, con M < N. Además nos dan una lista L de pares de estos
%estudiantes que son incompatibles entre sı́ (por ejemplo, porque siempre se copian). Queremos
%obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
%con M de los N estudiantes tal que si {x, y} ∈ L entonces {x, y} !⊆ S. Por ejemplo, una solución de
%li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
%es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1] .
%Escribe una versión lo más sencilla que puedas, aunque sea ineficiente, del estilo “generar una
%solución (total) y después comprobar si es correcta”.

generateList(0, []).
generateList(N, [N|List]):- N1 is N-1, generateList(N1, List), !.

esCorrecta(_, []).
esCorrecta(Sol, [Par|L]):-
    \+intersec(Par, Sol, Par),
    esCorrecta(Sol, L).

li(N, M, L, Sol):-
    generateList(N, Numbers), 
    subsetOfSize(M, Numbers, Sol),
    esCorrecta(Sol, L). 

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

