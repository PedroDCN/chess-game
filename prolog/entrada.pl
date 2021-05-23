:-[ajuda].

pegaMovimento(X,Y,X1,Y1):-
	write(' é sua vez! '),nl,ajuda(),nl,write('Qual peça deseja mover: '),nl,
	read(X),nl,write('Para qual casa deseja mover: '),nl,
	read(Y),
	casa(X,X1),
	casa(Y,Y1).

