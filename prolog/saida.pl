imprimeTabuleiro(T):-
	printTabuleiro(T,0,""),
	write(' A  B  C  D  E  F  G  H').

printTabuleiroComCoordenadas([], C, StringTabuleiro) :- C =:= 64, write(StringTabuleiro), nl, getColunas(Colunas), nl, write(Colunas), nl.
printTabuleiroComCoordenadas([X|Y], C, StringTabuleiro) :- 
  M is C + 1,
  (C mod 8 =:= 0 -> getLinhaFromIndice(C, X, L), write(StringTabuleiro), nl, espacamento(L, C, V), printTabuleiroComCoordenadas(Y, M, V);
  atom_concat(StringTabuleiro, X, R), espacamento(R, C, Z), printTabuleiroComCoordenadas(Y, M, Z)).

getLinhaFromIndice(0, Peca, R) :- atom_concat('8   ', Peca, R).
getLinhaFromIndice(8, Peca, R) :- atom_concat('7   ', Peca, R).
getLinhaFromIndice(16, Peca, R) :- atom_concat('6   ', Peca, R).
getLinhaFromIndice(24, Peca, R) :- atom_concat('5   ', Peca, R).
getLinhaFromIndice(32, Peca, R) :- atom_concat('4   ', Peca, R).
getLinhaFromIndice(40, Peca, R) :- atom_concat('3   ', Peca, R).
getLinhaFromIndice(48, Peca, R) :- atom_concat('2   ', Peca, R).
getLinhaFromIndice(56, Peca, R) :- atom_concat('1   ', Peca, R).
getLinhaFromIndice(_, Peca, Peca).

getColunas(R) :- atomic_list_concat(['    a', '  b', '  c', '  d', '  e', '  f', '  g', '  h'], ' ', R).

imprimeTurno(T1):- T1 == b -> write('Branco'); write('Preto').


menu():-
	write('************************************'),nl,
	write('*          Jogo de Xadrez          *'),nl,
	write('*                                  *'),nl,
	write('************************************'),nl.
	
	
casa(a1,R):- R is 56.
casa(a2,R):- R is 48.
casa(a3,R):- R is 40.
casa(a4,R):- R is 32.
casa(a5,R):- R is 24.
casa(a6,R):- R is 16.
casa(a7,R):- R is 8.
casa(a8,R):- R is 0.

casa(b1,R):- R is 57.
casa(b2,R):- R is 49.
casa(b3,R):- R is 41.
casa(b4,R):- R is 33.
casa(b5,R):- R is 25.
casa(b6,R):- R is 17.
casa(b7,R):- R is 9.
casa(b8,R):- R is 1.

casa(c1,R):- R is 58.
casa(c2,R):- R is 50.
casa(c3,R):- R is 42.
casa(c4,R):- R is 34.
casa(c5,R):- R is 26.
casa(c6,R):- R is 18.
casa(c7,R):- R is 10.
casa(c8,R):- R is 2.

casa(d1,R):- R is 59.
casa(d2,R):- R is 51.
casa(d3,R):- R is 43.
casa(d4,R):- R is 35.
casa(d5,R):- R is 27.
casa(d6,R):- R is 19.
casa(d7,R):- R is 11.
casa(d8,R):- R is 3.

casa(e1,R):- R is 60.
casa(e2,R):- R is 52.
casa(e3,R):- R is 44.
casa(e4,R):- R is 36.
casa(e5,R):- R is 28.
casa(e6,R):- R is 20.
casa(e7,R):- R is 12.
casa(e8,R):- R is 4.

casa(f1,R):- R is 61.
casa(f2,R):- R is 53.
casa(f3,R):- R is 45.
casa(f4,R):- R is 37.
casa(f5,R):- R is 29.
casa(f6,R):- R is 21.
casa(f7,R):- R is 13.
casa(f8,R):- R is 5.

casa(g1,R):- R is 62.
casa(g2,R):- R is 54.
casa(g3,R):- R is 46.
casa(g4,R):- R is 38.
casa(g5,R):- R is 30.
casa(g6,R):- R is 22.
casa(g7,R):- R is 14.
casa(g8,R):- R is 6.

casa(h1,R):- R is 63.
casa(h2,R):- R is 55.
casa(h3,R):- R is 47.
casa(h4,R):- R is 39.
casa(h5,R):- R is 31.
casa(h6,R):- R is 23.
casa(h7,R):- R is 15.
casa(h8,R):- R is 7.





