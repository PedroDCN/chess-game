module LogicaJogar where

import Tipos
import Xadrez
import LogicaValidar

-- alterna a vezDeQuem no jogo
mudaVez :: Jogo -> Jogo
mudaVez jo = Jogo (tabuleiro jo) (alternaCor (vezDeQuem jo)) (reiBrancoMov jo) (reiPretoMov jo) (torreBrancaEMov jo) (torreBrancaDMov jo) (torrePretaEMov jo) (torrePretaDMov jo)

-- muda tabuleiro dentro de um jogo
setNovoTab :: Jogo -> Tabuleiro -> Jogo
setNovoTab jo tab = Jogo tab (vezDeQuem jo) (reiBrancoMov jo) (reiPretoMov jo) (torreBrancaEMov jo) (torreBrancaDMov jo) (torrePretaEMov jo) (torrePretaDMov jo)

fazerJogada :: Jogada -> Jogo -> Jogo -- jogada de movimento normal -- retorna um novo jogo com o tabuleiro depois da jogada feita e a vez da cor oposta
fazerJogada jog@(Jogada pi pf) jo = 
    if verificaMovimento jog jo then
        mudaVez $ setNovoTab jo $ fazJogada jog jo
    else jo

getVezDeQuem :: Jogo -> Cor
getVezDeQuem = vezDeQuem