%Ejercicio 1
padre(bisabuelo,antonio).
padre(antonio,sergio).
padre(antonio,nestor).
padre(elpadreadriana,adriana).
padre(nestor,milena).
padre(antonio,lilian).
padre(hector,adrian).
padre(adrian,yamil).

madre(estela,sergio).
madre(estela,nestor).
madre(aurora,adriana).
madre(estela,lilian).
madre(lilian,yamil).
madre(elena,adrian).


personaReal(X):- padre(_,X),madre(_,X).

hijo(X,Y):-padre(Y,X);madre(Y,X).


hermano_de(estela,aurora).
hermano_de(X,Y):- padre(P,X),padre(P,Y); madre(M,X),madre(M,Y).
hermano(X,Y):- hermano_de(Y,X).
hermano(X,Y):- hermano_de(X,Y).

primo(X,Y):- padre(P,X),hermano(P,H),padre(H,Y) ; madre(M,X),hermano(M,H),madre(H,Y).

abuelo(X,Y):- hijo(M,X),hijo(Y,M).

ancestro(X,X).
ancestro(X,Y):- hijo(Y,P),ancestro(X,P).

%Ejercicio 2
suma(X,Y,Z ):- Z is X+Y.
producto(X,Y,Z) :- Z is X*Y.
factorial(0,1).
factorial(X,Y) :- X>0, X2 is X-1, factorial(X2,Z), producto(X,Z,Y).


%Ejercicio 3
rango(I,F,N) :- N>=I, N=<F.
%rango(4,7,6)
% 6>=4, 6<=7
% true, true
% true

%rango(4,7,9)
% 9>=4, 9=<7
% true, false
% false
%
% rango(4,7,X)
% X>=4,X=<7
% X no instanciada
% Error

rangoList(I,F,[X]) :- I==F, X==I.
rangoList(I,F,[X|XS]) :- I=X, I=<F, I2 is I+1, rangoList(I2,F,XS).

%Ejercicio 4
long([],0).
long([_|XS],L) :- L2 is L-1, long(XS,L2).

sumaLista([X],S) :- S==X.
sumaLista([X|XS],S) :- R is S-X, sumaLista(XS,R).

member([X|_],E) :- E==X.
member([X],E) :- E==X.
member([_|XS],E) :- member(XS,E).

append([],[],[]).
append([],[Y|YS],[Z|ZS]) :- Y==Z, append([],YS,ZS).
append([X|XS],[Y|YS],[Z|ZS]) :- X==Z, append(XS,[Y|YS],ZS).

%append([], Xs, Xs).
%append([Y|Ys], Xs, [Y|Zs]) :- append(Ys, Xs, Zs).

tomar([X|_],1,[Y|_]) :- X == Y.
tomar([X|XS], N, [Y|YS]) :- X == Y, N2 is N - 1, tomar(XS, N2, YS).

term([X|_],1,E) :- X == E.
term([_|XS],I,E) :- I2 is I-1, term(XS,I2,E).

flat([], []).
flat([[]|XSS],YS) :- flat(XSS,YS).
flat([[X|XS]|XSS], [Y|YS]) :- X==Y, flat([XS|XSS], YS).

maxl([],_).
maxl([X|XS],N) :- X=<N, maxl(XS,N). 

%Ejercicio 5

%Ejercicio 6
termMat([[X|_]|_],1,1,E) :- X == E.
termMat([[_|XS]|XSS],1,C,E) :- C2 is C-1, termMat([XS|XSS],1,C2,E).
termMat([_|XSS],F,C,E) :- F2 is F-1, termMat(XSS,F2,C,E).