trocaPeca([], 64, _, _, _, _, []).
trocaPeca([X|Y], C, PecaO, PecaD, IndiceOrigem, IndiceDestino, [XR|YR]) :-
  C < 64,
  M is C + 1,
  vazio(_V, _),
  (C == IndiceOrigem -> XR = '\u25A1';
  C == IndiceDestino -> XR = PecaO;
  XR = X),
  trocaPeca(Y, M, PecaO, PecaD, IndiceOrigem, IndiceDestino, YR).


verificaExistePecaMesmaCor(Tabuleiro, IndiceOrigem, IndiceDestino) :-
  pegaPeca(IndiceOrigem, Tabuleiro, PecaO),
  pegaPeca(IndiceDestino, Tabuleiro, PecaD),
  (branco(PecaO, _) -> branco(PecaD, _);
  preto(PecaO, _) -> preto(PecaD, _)).

troca(Tabuleiro, IndiceOrigem, IndiceDestino, TabuleiroR) :-
  \+ verificaExistePecaMesmaCor(Tabuleiro, IndiceOrigem, IndiceDestino),
  pegaPeca(IndiceOrigem, Tabuleiro, PecaO),
  pegaPeca(IndiceDestino, Tabuleiro, PecaD),
  trocaPeca(Tabuleiro, 0, PecaO, PecaD, IndiceOrigem, IndiceDestino, TabuleiroR).

:- [tabuleiro].

movePeca(X,Y,Xn,Yn,T,Tn) :-
  pegaPecaAt(X,Y,T,P),
  (verificaMovimentoPeca(X,Y,Xn,Yn,T) -> 
    setPecaAt(Xn,Yn,P,T,Tf),
    setPecaAt(X,Y,v,Tf,Tfinal),
    Tn = Tfinal
  ; writeln('\nmovimento invalido'),Tn = T).

listadecasasDSE(_,8,_,_,_,[]).
listadecasasDSE(_,_,-1,_,_,[]).
listadecasasDSE(Xi,X,Y,Cor,T,R) :- % lista de casas da diagonal superior esquerda a partir de (x,y)
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;
    
    S = [[X,Y]],
    Xn is X+1, Yn is Y-1,
    listadecasasDSE(Xi,Xn,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasDSD(_,8,_,_,_,[]).
listadecasasDSD(_,_,8,_,_,[]).
listadecasasDSD(Xi,X,Y,Cor,T,R) :- % lista de casas da diagonal superior direita a partir de (x,y)
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;
    
    S = [[X,Y]],
    Xn is X+1, Yn is Y+1,
    listadecasasDSD(Xi,Xn,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasDIE(_,-1,_,_,_,[]).
listadecasasDIE(_,_,-1,_,_,[]).
listadecasasDIE(Xi,X,Y,Cor,T,R) :- % lista de casas da diagonal inferior esquerda a partir de (x,y)
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;

    S = [[X,Y]],
    Xn is X-1, Yn is Y-1,
    listadecasasDIE(Xi,Xn,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasDID(_,-1,_,_,_,[]).
listadecasasDID(_,_,8,_,_,[]).
listadecasasDID(Xi,X,Y,Cor,T,R) :- % lista de casas da diagonal inferior direita a partir de (x,y)
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;

    S = [[X,Y]],
    Xn is X-1, Yn is Y+1,
    listadecasasDID(Xi,Xn,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasHe(_,_,-1,_,_,[]).
listadecasasHe(Yi,X,Y,Cor,T,R) :-
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, Y \== Yi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;

    S = [[X,Y]],
    Yn is Y-1,
    listadecasasHe(Yi,X,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasHd(_,_,8,_,_,[]).
listadecasasHd(Yi,X,Y,Cor,T,R) :-
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, Y \== Yi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;

    S = [[X,Y]],
    Yn is Y+1,
    listadecasasHd(Yi,X,Yn,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasVc(_,8,_,_,_,[]).
listadecasasVc(Xi,X,Y,Cor,T,R) :-
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;
        
     S = [[X,Y]],
     Xn is X+1,
     listadecasasVc(Xi,Xn,Y,Cor,T,R2),
     append(S,R2,L),
     R = L).

listadecasasVb(_,-1,_,_,_,[]).
listadecasasVb(Xi,X,Y,Cor,T,R) :-
    pegaPecaAt(X,Y,T,P),
    cor(P,C),
    ((Cor == C, X \== Xi) -> R = [];
     (coroposta(Cor,C)) -> 
     S = [[X,Y]],
     R = S;

    S = [[X,Y]],
    Xn is X-1,
    listadecasasVb(Xi,Xn,Y,Cor,T,R2),
    append(S,R2,L),
    R = L).

listadecasasTorre(X,Y,T,R) :-
  pegaPecaAt(X,Y,T,P),
  cor(P,C),
  listadecasasHe(Y,X,Y,C,T,R1),
  listadecasasHd(Y,X,Y,C,T,R2),
  listadecasasVc(X,X,Y,C,T,R3),
  listadecasasVb(X,X,Y,C,T,R4),
  R1 = [_|T1],
  R2 = [_|T2],
  R3 = [_|T3],
  R4 = [_|T4],
  append(T1,T2,S1),
  append(S1,T3,S2),
  append(S2,T4,S3),
  R = S3,!.

% R é a lista de todas as posições (x,y) que a bispo pode alcançar partindo da posição inicial (X,Y).
listadecasasBispo(X,Y,T,R) :-
  pegaPecaAt(X,Y,T,P),
  cor(P,C),
  listadecasasDSE(X,X,Y,C,T,R1),
  listadecasasDSD(X,X,Y,C,T,R2),
  listadecasasDIE(X,X,Y,C,T,R3),
  listadecasasDID(X,X,Y,C,T,R4),
  R1 = [_|T1],
  R2 = [_|T2],
  R3 = [_|T3],
  R4 = [_|T4],
  append(T1,T2,S1),
  append(S1,T3,S2),
  append(S2,T4,S3),
  R = S3,!.

listadecasasDama(X,Y,T,R) :-
  listadecasasBispo(X,Y,T,R1),
  listadecasasTorre(X,Y,T,R2),
  append(R1,R2,R3),
  R = R3,!.

listadecasasCavalo(X,Y,T,R) :-
  pegaPecaAt(X,Y,T,P),
  cor(P,C),

  ((Y =\= 0 , X =\= 6 , X =\= 7) ->
      Xv1 is X + 2, Yv1 is Y - 1,
      Dse1 = [Xv1,Yv1]; Dse1 = []),
  (Dse1 == [] -> Possiveldse1 = [];
      verificaSeTemInimigo(Dse1,C,T,TemInimigo1),
      verificaSeCasaVazia(Dse1,T,Vazia1),
      ((TemInimigo1;Vazia1) -> Possiveldse1 = [Dse1]; Possiveldse1 = [])          
      ),

  ((Y =\= 0 , Y =\= 1 , X =\= 7) ->
      Xv2 is X + 1, Yv2 is Y - 2,
      Dse2 = [Xv2,Yv2]; Dse2 = []),
  (Dse2 == [] -> Possiveldse2 = [];
      verificaSeTemInimigo(Dse2,C,T,TemInimigo2),
      verificaSeCasaVazia(Dse2,T,Vazia2),
      ((TemInimigo2;Vazia2) -> Possiveldse2 = [Dse2]; Possiveldse2 = [])
      ),

  ((Y =\= 7 , X =\= 6 , X =\= 7) ->
      Xv3 is X + 2, Yv3 is Y + 1,
      Dsd1 = [Xv3,Yv3]; Dsd1 = []),
  (Dsd1 == [] -> Possiveldsd1 = [];
      verificaSeTemInimigo(Dsd1,C,T,TemInimigo3),
      verificaSeCasaVazia(Dsd1,T,Vazia3),
      ((TemInimigo3;Vazia3) -> Possiveldsd1 = [Dsd1]; Possiveldsd1 = [])
      ),
  
  ((Y =\= 6 , Y =\= 7 , X =\= 7) ->
      Xv4 is X + 1, Yv4 is Y + 2,
      Dsd2 = [Xv4,Yv4]; Dsd2 = []),
  (Dsd2 == [] -> Possiveldsd2 = [];
      verificaSeTemInimigo(Dsd2,C,T,TemInimigo4),
      verificaSeCasaVazia(Dsd2,T,Vazia4),
      ((TemInimigo4;Vazia4) -> Possiveldsd2 = [Dsd2]; Possiveldsd2 = [])
      ),
  
  ((Y =\= 0 , X =\= 1 , X =\= 0) ->
      Xv5 is X - 2, Yv5 is Y - 1,
      Die1 = [Xv5,Yv5]; Die1 = []),
  (Die1 == [] -> Possiveldie1 = [];
      verificaSeTemInimigo(Die1,C,T,TemInimigo5),
      verificaSeCasaVazia(Die1,T,Vazia5),
      ((TemInimigo5;Vazia5) -> Possiveldie1 = [Die1]; Possiveldie1 = [])
      ),
  
  ((Y =\= 0 , Y =\= 1 , X =\= 0) ->
      Xv6 is X - 1, Yv6 is Y - 2,
      Die2 = [Xv6,Yv6]; Die2 = []),
  (Die2 == [] -> Possiveldie2 = [];
      verificaSeTemInimigo(Die2,C,T,TemInimigo6),
      verificaSeCasaVazia(Die2,T,Vazia6),
      ((TemInimigo6;Vazia6) -> Possiveldie2 = [Die2]; Possiveldie2 = [])
      ),

  ((Y =\= 7 , X =\= 1 , X =\= 0) ->
      Xv7 is X - 2, Yv7 is Y + 1,
      Did1 = [Xv7,Yv7]; Did1 = []),
  (Did1 == [] -> Possiveldid1 = [];
      verificaSeTemInimigo(Did1,C,T,TemInimigo7),
      verificaSeCasaVazia(Did1,T,Vazia7),
      ((TemInimigo7;Vazia7) -> Possiveldid1 = [Did1]; Possiveldid1 = [])
      ),

  ((Y =\= 6 , Y =\= 7 , X =\= 0) ->
      Xv8 is X - 1, Yv8 is Y + 2,
      Did2 = [Xv8,Yv8]; Did2 = []),
  (Did2 == [] -> Possiveldid2 = [];
      verificaSeTemInimigo(Did2,C,T,TemInimigo8),
      verificaSeCasaVazia(Did2,T,Vazia8),
      ((TemInimigo8;Vazia8) -> Possiveldid2 = [Did2]; Possiveldid2 = [])
      ),
  
  append(Possiveldse1,Possiveldse2,R1),
  append(R1,Possiveldsd1,R2),
  append(R2,Possiveldsd2,R3),
  append(R3,Possiveldie1,R4),
  append(R4,Possiveldie2,R5),
  append(R5,Possiveldid1,R6),
  append(R6,Possiveldid2,R7),

  R = R7,!.

listadecasasPeao(X,Y,T,R) :-
  pegaPecaAt(X,Y,T,P),
  cor(P,Cor),
  % se o peão for branco
  (Cor == b -> 
      listadecasasVc(X,X,Y,Cor,T,R1),  
      listadecasasDSE(X,X,Y,Cor,T,R2),
      listadecasasDSD(X,X,Y,Cor,T,R3),
      R1 = [_|T1],
      R2 = [_|T2],
      R3 = [_|T3],
      pegaCabeca(T1,C11),
      pegaCabeca(T2,C2),
      pegaCabeca(T3,C3),
      length(C11,TamC11),
      (TamC11 =\= 0 ->
          C11 = [C111|_],
          verificaSeCasaVazia(C111,T,Vazia1),
          (Vazia1 -> append(C11,[],C1) ; append([],[],C1))
      ; append([],[],C1)),
      append(C1,[],S1),
      length(C2,Tam1), length(C3,Tam2),
      (Tam1 =\= 0 -> % se tiver a casa
          C2 = [C21|_],
          verificaSeTemInimigo(C21,Cor,T,TemInimigo1),
          (TemInimigo1 -> append(C2,S1,S2)
              ; append([],S1,S2))
      ; append([],S1,S2)),
      (Tam2 =\= 0 ->
          C3 = [C31|_],
          verificaSeTemInimigo(C31,Cor,T,TemInimigo2),
          (TemInimigo2 -> append(C3,S2,S3)
              ; append([],S2,S3))
      ; append([],S2,S3)),
      length(C1,TamC1),
      ( (X=:=1,TamC1=\=0) -> 
              Xn is X + 2,
              verificaSeCasaVazia([Xn,Y],T,Vazia2),
              (Vazia2 -> append([[Xn,Y]],S3,S4) ; append([],S3,S4))
              ; append([],S3,S4)),
      R = S4
  ; % se o peão for preto
      listadecasasVb(X,X,Y,Cor,T,R4),
      listadecasasDIE(X,X,Y,Cor,T,R5),
      listadecasasDID(X,X,Y,Cor,T,R6),
      R4 = [_|T4],
      R5 = [_|T5],
      R6 = [_|T6],
      pegaCabeca(T4,C41),
      pegaCabeca(T5,C5), 
      pegaCabeca(T6,C6),
      length(C41,TamC41),
      (TamC41 =\= 0 ->
          C41 = [C411|_],
          verificaSeCasaVazia(C411,T,Vazia3),
          (Vazia3 -> append(C41,[],C4) ; append([],[],C4))
      ; append([],[],C4)),
      append(C4,[],S4),
      length(C5,Tam3), length(C6,Tam4),
      (Tam3 =\= 0 -> % se tiver a casa
          C5 = [C51|_],
          verificaSeTemInimigo(C51,Cor,T,TemInimigo3),
          (TemInimigo3 -> append(C5,S4,S5)
              ; append([],S4,S5))
      ; append([],S4,S5)),
      (Tam4 =\= 0 ->
          C6 = [C61|_],
          verificaSeTemInimigo(C61,Cor,T,TemInimigo4),
          (TemInimigo4 -> append(C6,S5,S6)
              ; append([],S5,S6))
      ; append([],S5,S6)),
      length(C4,TamC4),
      ( (X=:=6,TamC4=\=0) -> 
              Xn is X - 2,
              verificaSeCasaVazia([Xn,Y],T,Vazia4),
              (Vazia4 -> append([[Xn,Y]],S6,S7) ; append([],S6,S7))
              ; append([],S6,S7)),
      R = S7
      ),!.

listadecasasRei(X,Y,T,R) :-
  pegaPecaAt(X,Y,T,P),
  cor(P,Cor),
  listadecasasDSE(X,X,Y,Cor,T,R1),
  listadecasasDSD(X,X,Y,Cor,T,R2),
  listadecasasDIE(X,X,Y,Cor,T,R3),
  listadecasasDID(X,X,Y,Cor,T,R4),
  listadecasasHe(Y,X,Y,Cor,T,R5),
  listadecasasHd(Y,X,Y,Cor,T,R6),
  listadecasasVc(X,X,Y,Cor,T,R7),
  listadecasasVb(X,X,Y,Cor,T,R8),
  R1 = [_|T1],
  R2 = [_|T2],
  R3 = [_|T3],
  R4 = [_|T4],
  R5 = [_|T5],
  R6 = [_|T6],
  R7 = [_|T7],
  R8 = [_|T8],
  pegaCabeca(T1,C1),
  pegaCabeca(T2,C2),
  pegaCabeca(T3,C3),
  pegaCabeca(T4,C4),
  pegaCabeca(T5,C5),
  pegaCabeca(T6,C6),
  pegaCabeca(T7,C7),
  pegaCabeca(T8,C8),
  append(C1,C2,S1),
  append(S1,C3,S2),   
  append(S2,C4,S3),
  append(S3,C5,S4),
  append(S4,C6,S5),
  append(S5,C7,S6),
  append(S6,C8,S7),
  R = S7,!.

pegaCabeca(Lista,ListaC) :-
  length(Lista, Tam),
  (Tam =:= 0 -> ListaC = []; 
      Lista = [C|_],
      ListaC = [C]).

verificaMovimentoPeca(Xi,Yi,Xf,Yf,T) :-
    pegaPecaAt(Xi,Yi,T,P),
    ( (P==pb;P==pp) -> listadecasasPeao(Xi,Yi,T,L1),member([Xf,Yf],L1)
    ; (P==tb;P==tp) -> listadecasasTorre(Xi,Yi,T,L2),member([Xf,Yf],L2)
    ; (P==cb;P==cp) -> listadecasasCavalo(Xi,Yi,T,L3),member([Xf,Yf],L3)
    ; (P==bb;P==bp) -> listadecasasBispo(Xi,Yi,T,L4),member([Xf,Yf],L4)
    ; (P==db;P==dp) -> listadecasasDama(Xi,Yi,T,L5),member([Xf,Yf],L5)
    ; (P==rb;P==rp) -> listadecasasRei(Xi,Yi,T,L6),member([Xf,Yf],L6)
    ; true),!.