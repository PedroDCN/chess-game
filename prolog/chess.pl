:- [tabuleiro].
:- [tipos].
:- [movimento].
:- [ajuda].
:-[entrada].
:-[saida].
:- encoding(utf8).

pegaPeca(Indice, Tabuleiro, Peca) :- nth0(Indice, Tabuleiro, Peca).
%% remove a peça da posição passada e adiciona "vazio" no lugar.
%% removePeca(Indice, Tabuleiro, NovoTabuleiro) :- 

%% move uma peça para uma determinada posição, precisa verificar:
%% Se já existe uma peça da mesma cor naquela posição (sugerir mudança para facilitar a implementação);
%% Se ela vai capturar uma outra peça;
%% Checar se a peça segue suas regras;
%% Checar se com o movimento de tal peça, o rei vai ficar em xeque
%% Etc...
%% movePeca(tabuleiro, indice, peca, novoTabuleiro) :-

printTabuleiro([], C, StringTabuleiro) :- C =:= 64, write(StringTabuleiro), nl.
printTabuleiro([X|Y], C, StringTabuleiro) :- M is C + 1, (C mod 8 =:= 0 -> write(StringTabuleiro), nl, espacamento(X, C, V), printTabuleiro(Y, M, V);
                                            atom_concat(StringTabuleiro, X, R), espacamento(R, C, Z), printTabuleiro(Y, M, Z)).

printIndices([], C, StringIndices) :- C =:= 64, write(StringIndices), nl.
printIndices([X|Y], C, StringIndices) :- M is C + 1, (C mod 8 =:= 0 -> write(StringIndices), nl, espacamentoIndices(X, C, V), printIndices(Y, M, V);
                                            atom_concat(StringIndices, X, R), espacamentoIndices(R, C, Z), printIndices(Y, M, Z)).

espacamento(String,_C, R) :- atom_concat(String, "   ", R).
espacamentoIndices(String, C, R) :- C < 10 -> atom_concat(String, "    ", R) ; atom_concat(String, "   ", R).

turno(b,p).
turno(p,b).

jogo:-
	menu2,
	tabuleiro(T), tabInicial(Ti),
	printTabuleiroComCoordenadas(T,0,""),nl,
	imprimeTurno(b),
	pegaMovimento(_X,_Y,X1,Y1,Xc,Yc),
	casaVetor(Xc,Xi,Yi), casaVetor(Yc,Xf,Yf),
	pegaPecaAt(Xi,Yi,Ti,P), cor(P,C),
	(C \== b -> writeln('\nNão é a sua vez!'),loop(T,Ti,b);
	( (X1 =:= -1 , Y1 =:= -1) -> loop(T,Ti,b);
	movePeca(Xi,Yi,Xf,Yf,Ti,Tn),
	(Ti == Tn -> loop(T,Ti,b);
	troca(T,X1,Y1,TR),
	turno(b,PrxTurno),
	loop(TR,Tn,PrxTurno)))).
	
	
loop(T,Tl,Turno):-
	nl,printTabuleiroComCoordenadas(T,0,""),nl,
	imprimeTurno(Turno),
	pegaMovimento(_X,_Y,X1,Y1,Xc,Yc),
	casaVetor(Xc,Xi,Yi), casaVetor(Yc,Xf,Yf),
	pegaPecaAt(Xi,Yi,Tl,P), cor(P,C),
	(C \== Turno -> writeln('\nNão é a sua vez!'),loop(T,Tl,Turno);
	( (X1 =:= -1 , Y1 =:= -1) -> loop(T,Tl,Turno);
	movePeca(Xi,Yi,Xf,Yf,Tl,Tn),
	(Tl == Tn -> loop(T,Tl,Turno);
	troca(T,X1,Y1,TR),
	turno(Turno,PrxTurno),
	loop(TR,Tn,PrxTurno)))). 
	
%% Testando o programa
%%main :-
    %%read(Indice),
    %%tabuleiro(Tabuleiro),
    %%pegaPeca(Indice, Tabuleiro, Peca).
    %%branco(Peca, _) -> write("A peça é branca") ; write("A peça é preta"),
