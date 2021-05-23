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
