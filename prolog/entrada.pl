:-[ajuda].

pegaMovimento(X,Y,X1,Y1,Xc,Yc):-
	write(' é sua vez! '),nl,ajuda(),nl,write('Qual peça deseja mover: '),nl,
	read(X),nl,write('Para qual casa deseja mover: '),nl,
	read(Y),tabuleiroComCoords(Tx),
	((member(X,Tx) , member(Y,Tx)) -> casa(X,X1),casa(Y,Y1),Xc=X,Yc=Y
	; writeln('\nCasa invalida!'),casa('000',X1),casa('000',Y1),Xc=X,Yc=Y
	).
	

