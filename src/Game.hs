module Game where

import Cor
import LogicaJogar
import LogicaValidar
import Movimento
import Pecas
import Posicao 
import Tabuleiro 

{--

modulo com funções principais de controle do jogo que irá conversar com a gui.

principais funções até o momento:

carregaTabuleiro:
retorna tabuleiro vazio.

criaJogo:
carrega tabuleiro na posição inicial do jogo.

fazJogada:
entrada de jogada do usuário, insere apenas a posição inicial da peça escolhida e sua posição final no estado de jogo atual.
a jogada só é realizada se for válida.
lógica por trás:
    verifica se a jogada é legal e válida de ser feita; (segue as regras de movimento e se é possível de ser feita na posição atual)
    tenta inserir a peça na posição final;
    remove a peça da posição inicial.

JogadasValidas:
dada uma posição no tabuleiro, retorna uma lista de todas os movimentos possíveis de serem feitos (válidos e legais) pela
peça naquela casa, caso tenha uma peça ([] caso seja uma casa vazia)



--------------------------------------------------------
sequencia de casos de uso de como o jogo ocorre:

- dois jogadores jogando:

1 o jogo começa com as peças na posição inicial.
2 começa pela vez das brancas.
3 brancas selecionam uma peça no tabuleiro, e selecionam a casa para onde a peça irá se mover.
4 se a jogada em questão for possível de acontecer (legal e válida dentro das condições da posição atual), a peça irá se mover, e a vez irá passar para as negras.
5 negras selecionam uma peça do tabuleiro, e selecionam a casa para onde a peça irá se mover.
6 se a jogada em questão for possível de acontecer (legal e válida dentro das condições da posição atual), a peça irá se mover, e a vez irá passar para as brancas.
7 volta para o passo 3
8 o jogo acaba quando for a vez de um dos dois lados, e essa cor estiver em cheque, e não possuir nenhuma jogada (legal e válida) para fazer na posição, ou seja, está em cheque mate.
9 o jogo também pode acabar quando for a vez de um dos dois lados, e essa cor não possuir nenhuma jogada (legal e válida) para fazer na posição, e não estiver em cheque, ou seja, empate por afogamento.
    (resumindo, se cor não tiver jogada para fazer, verifica se está em cheque, caso esteja, é mate, caso não, empate)

- jogador contra computador:

1 o jogo começa com as peças na posição inicial.
2 começa pela vez das brancas.
3 o jogador (que sempre começa de brancas) seleciona uma peça no tabuleiro, e seleciona a casa para onde a peça irá se mover.
4 se a jogada em questão for possível de acontecer (legal e válida dentro das condições da posição atual), a peça irá se mover, e a vez irá passar para as negras.
5 o estado atual do jogo (posição atual do tabuleiro) é passado para uma função, que retorna uma jogada a ser feita pelas negras. e a vez irá passar para as brancas.
        é garantida pela função que a jogada é legal e válida dentro das condições da posição atual
6 volta para o passo 3
7 o jogo acaba quando for a vez de um dos dois lados, e essa cor estiver em cheque, e não possuir nenhuma jogada (legal e válida) para fazer na posição, ou seja, está em cheque mate.
8 o jogo também pode acabar quando for a vez de um dos dois lados, e essa cor não possuir nenhuma jogada (legal e válida) para fazer na posição, e não estiver em cheque, ou seja, empate por afogamento.
    (resumindo, se cor não tiver jogada para fazer, verifica se está em cheque, caso esteja, é mate, caso não, empate)
--}