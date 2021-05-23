:-[saida].


getDescricaoPecas() :-
  pecasBrancas(PB),
  pecasPretas(PP),
  getDescPecasBrancas(PB),
  nl,
  getDescPecasPretas(PP).


getDescPecasBrancas([]).
getDescPecasBrancas([X|Y]) :-
  branco(X, R),
  atomic_list_concat([X, R], ' - ', RD),
  write(RD), nl,
  getDescPecasBrancas(Y).

getDescPecasPretas([]).
getDescPecasPretas([X|Y]) :-
  preto(X, R),
  atomic_list_concat([X, R], ' - ', RD),
  write(RD), nl,
  getDescPecasPretas(Y).



ajuda() :-
  write('Escolha uma das opções:'), nl,
  write('1 para Glossário | 2 para Visualizar Coordenadas do Tabuleiro | 3 Continuar Jogando | 4 Sair do jogo'), nl,
  read(INPUT),
  (INPUT == 1 -> getDescricaoPecas(), nl, (querVoltar() -> ajuda(); true);
  INPUT == 2 -> tabuleiroComCoords(T), printTabuleiro(T, 0, ""), nl, (querVoltar() -> ajuda(); true);
  INPUT == 3 -> true;
  INPUT == 4 -> halt;
  write('Opção inválida.'), ajuda()).

querVoltar() :-
  write('Deseja voltar para a tela de ajuda? 1 - Sim | 2 - Não'), nl,
  read(INPUT),
  (INPUT == 1 -> true;
  false).
