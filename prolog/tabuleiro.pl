tabuleiro(T) :- T = ['\u2656','\u2658', '\u2657','\u2655', '\u2654','\u2657', '\u2658', '\u2656',
                     '\u2659', '\u2659','\u2659', '\u2659', '\u2659', '\u2659', '\u2659', '\u2659',
                      '\u25A1','\u25A1','\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1',
                      '\u25A1','\u25A1','\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1',
                      '\u25A1','\u25A1','\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1',
                     '\u25A1','\u25A1','\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1', '\u25A1',
                      '\u265F', '\u265F', '\u265F', '\u265F', '\u265F', '\u265F', '\u265F', '\u265F',
                     '\u265C', '\u265E', '\u265D', '\u265B', '\u265A','\u265D', '\u265E', '\u265C'].

tabuleiroComCoords(T) :- T = [a8, b8, c8, d8, e8, f8, g8, h8,
                              a7, b7, c7, d7, e7, f7, g7, h7,
                              a6, b6, c6, d6, e6, f6, g6, h6,
                              a5, b5, c5, d5, e5, f5, g5, h5,
                              a4, b4, c4, d4, e4, f4, g4, h4,
                              a3, b3, c3, d3, e3, f3, g3, h3,
                              a2, b2, c2, d2, e2, f2, g2, h2,
                              a1, b1, c1, d1, e1, f1, g1, h1].

%torreP = tP
%cavaloP = cP
%bispoP = bP
%damaP = dP
%reiP = rP
%peaoP = pP

%torreB = tB
%cavaloB = cB
%bispoB = bB
%damaB = dB
%reiB = rB
%peaoB = pB


indicesValidos(I) :- I = [  0,  1,  2,  3,  4,  5,  6,  7,
                            8,  9, 10, 11, 12, 13, 14, 15,
                           16, 17, 18, 19, 20, 21, 22, 23,
                           24, 25, 26, 27, 28, 29, 30, 31,
                           32, 33, 34, 35, 36, 37, 38, 39,
                           40, 41, 42, 43, 44, 45, 46, 47,
                           48, 49, 50, 51, 52, 53, 54, 55,
                           56, 57, 58, 59, 60, 61, 62, 63].

peca(tb).
peca(cb).
peca(bb).
peca(db).
peca(rb).
peca(pb).
peca(tp).
peca(cp).
peca(bp).
peca(dp).
peca(rp).
peca(pp).

cor(tb,b). % torre branca é branca
cor(cb,b). % cavalo branco é branco
cor(bb,b). % bispo branco é branco
cor(db,b). % dama branca é branca
cor(rb,b). % rei branco é branco
cor(pb,b). % peão branco é branco
cor(tp,p). % torre preta é preta
cor(cp,p). % cavalo preto é preto
cor(bp,p). % bispo preto é preto
cor(dp,p). % dama preta é preta
cor(rp,p). % rei preto é preto
cor(pp,p). % peão preto é preto
cor(v,s). % casa vazia Sem cor

vazio(v). % casa vazia V

coroposta(b,p).
coroposta(p,b).
coroposta(s,s).

tabInicial(T) :- T = [  [tp,cp,bp,dp,rp,bp,cp,tp], % tabuleiro na posição inicial
                [pp,pp,pp,pp,pp,pp,pp,pp],
                [v,v,v,v,v,v,v,v],
                [v,v,v,v,v,v,v,v],
                [v,v,v,v,v,v,v,v],
                [v,v,v,v,v,v,v,v],
                [pb,pb,pb,pb,pb,pb,pb,pb],
                [tb,cb,bb,db,rb,bb,cb,tb]
            ].

% pega a peça no tabuleiro T que está na posição (X,Y) e salva em P
pegaPecaAt(X,Y,T,P) :-
    Xn is 7 - X,
    nth0(Xn, T, Linha),
    nth0(Y, Linha, Pr),
    P = Pr.

removeAt(As,N1,Bs) :-
    same_length(As,[_|Bs]),
    append(Prefix,[_|Suffix],As),
    length(Prefix,N1),
    append(Prefix,Suffix,Bs).

insertAt(E,N,Xs,Ys) :-
    same_length([E|Xs],Ys),
    append(Before,Xs0,Xs),
    length(Before,N),
    append(Before,[E|Xs0],Ys).

% predicado que coloca na posição X,Y do tabuleiro T a peça P, salvando o novo tabuleiro em Tn
setPecaAt(X,Y,P,T,Tn) :-
    Xn is 7 - X,
    nth0(Xn,T,Linha),
    
    % mudo a casa na posição Y da linha
    removeAt(Linha,Y,NLinha),
    insertAt(P,Y,NLinha,NovaLinha),

    % mudo a linha X com a casa alterada
    removeAt(T,Xn,Ntab),
    insertAt(NovaLinha,Xn,Ntab,NovoTabuleiro),

    Tn = NovoTabuleiro.

verificaSeTemInimigo(CasaXY,Cor,T,TemInimigo) :-
    CasaXY = [X,Y|_],
    pegaPecaAt(X,Y,T,P),
    coroposta(Cor,Coroposta),
    cor(P,C),
    (C == Coroposta -> TemInimigo = true;
    TemInimigo = false).

verificaSeCasaVazia(CasaXY,T,Vazia) :-
    CasaXY = [X,Y|_],
    pegaPecaAt(X,Y,T,P),
    (vazio(P) -> Vazia = true;
    Vazia = false).