module Tabuleiro where

import  Tipos
{-- 
-- tabuleiro é uma lista de quadrados, ind 0 = a1, ind 63 = h8
-- ou
-- tabuleiro é uma lista de 8 listas, cada um com 8 casas
-- primeira lista primeiro elemento a1, ultima lista ultimo elemento h8
--
-- também possui um dado que guarda a Cor do próximo a jogar
--}

{--
-- uma função que muda a vez do próximo a jogar
-- (muda a Cor do próximo no tabuleiro)
--
-- necessário ter uma função que retorna
-- o tabuleiro da posição atual
-- uma função que retorna um tabuleiro vazio
-- e uma função que retorna a posição inicial do jogo
--
-- uma função que acessa uma posição (x,y) do tabuleiro
-- retorna o que tiver nele (talvez uma peça)
--
-- uma função que (talvez) coloca uma peça no tabuleiro. Se
-- a posição não for válida ela não coloca
--
-- uma função que remove uma peça de uma posição do tabuleiro.
--
--
-- obs: funções de acesso usam o tipo posição do modulo Posicao.
--}



{- Utilitários do Quadrado -}

-- obter a cor de um quadrado
getCorQuadrado  ::  Quadrado  ->  CorPeca
getCorQuadrado( Peca cp tp) = cp
getCorQuadrado ( Vazio )        =  SemCor

-- obter tipo de quadrado
getTipoQuadrado  ::  Quadrado  ->  TipoPeca
getTipoQuadrado  ( Peca cp tp) = tp
getTipoQuadrado  ( Vazio )        =  SemTipo

getPTipoQuadrado ::  TipoPeca  ->  String
getPTipoQuadrado tp =  case tp of
    Bispo     ->  " Bispo "
    Rei       ->  " Rei "
    Cavaleiro ->  " Cavaleiro "
    Peão      ->  " Peão "
    Rainha    ->  " Rainha "
    Torre     ->  " Torre "
    otherwise ->  " SemTipo "
