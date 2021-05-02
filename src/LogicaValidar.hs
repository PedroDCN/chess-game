module LogicaValidar where

import Tipos
import Xadrez

verificaMovimento :: Jogada -> Jogo -> Bool -- peão esta movendo em diagonal mesmo sem peça -- bispo esta pulando peças aliadas mas n para em cima delas -- rei totalmente quebrado
verificaMovimento jog@(Jogada posi posf) jo
  | tipopeca == Bispo = verificaMovimentoBispo jog jo
  | tipopeca == Torre = verificaMovimentoTorre jog jo
  | tipopeca == Dama = verificaMovimentoDama jog jo
  | tipopeca == Cavalo = verificaMovimentoCavalo jog jo
  | tipopeca == Peao = verificaMovimentoPeao jog jo
  | tipopeca == Rei = verificaMovimentoRei jog jo
  | otherwise = False
  where
      tipopeca = tipo $ getPecaPosicao posi jo

-- Função que verifica se uma jogada feito com o bispo para o momento do jogo atual é válida
verificaMovimentoBispo :: Jogada -> Jogo -> Bool
verificaMovimentoBispo (Jogada posi posf) jo = lista /= [] && (posf `elem` lista)
    where lista = listaTodasJogadasBispo posi jo

-- Função que lista, para uma posição dada em um jogo, todas as possíveis jogadas do bispo estando naquela casa
listaTodasJogadasBispo :: Posicao -> Jogo -> [Posicao]
listaTodasJogadasBispo posi@(x1,y1) jo = possiveisID ++ possiveisIE ++ possiveisSD ++ possiveisSE
    where
        corBispo = getCorPosicao posi jo

        xscima = if x1+1 /= 8 then [(x1+1)..7] else []              -- todas as linhas para cima da casa
        xsbaixo = if x1-1 /= -1 then reverse [0..(x1-1)] else []    -- todas as linhas para baixo da casa
        ysesquerda = if y1-1 /= -1 then reverse [0..(y1-1)] else [] -- todas as colunas a esquerda da casa
        ysdireita = if y1+1 /= 8 then [(y1+1)..7] else []           -- todas as colunas a direita da casa

        -- todos listas de todas as posições
        casasSE = zip xscima ysesquerda                             -- todas as casas na diagonal Superior Esquerda da casa
        casasSD = zip xscima ysdireita                              -- todas as casas na diagonal Superior Direita da casa
        casasIE = zip xsbaixo ysesquerda                            -- todas as casas na diagonal Inferior Esquerda da casa
        casasID = zip xsbaixo ysdireita                             -- todas as casas na diagonal Inferior Direita da casa

        -- todos listas de posições possíveis
        possiveisSE = if null casasSE then [] else                                                                                                 -- todas as casas válidas na diagonal superior Esquerda
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasSE in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corBispo]

        possiveisSD = if null casasSD then [] else                                                                                                 -- todas as casas válidas na diagonal superior direita
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasSD in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corBispo]

        possiveisIE = if null casasIE then [] else                                                                                                 -- todas as casas válidas na diagonal inferior esquerda
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasIE in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corBispo]
        possiveisID = if null casasID then [] else                                                                                                 -- todas as casas válidas na diagonal inferior direita
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasID in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corBispo]

        --possiveisID = if null casasID then [] else foldl (\acc x ->
        --   (getPecaPosicao x jo)) [] casasID

-- Função que verifica se uma jogada feito com a torre para o momento do jogo atual é válida
verificaMovimentoTorre :: Jogada -> Jogo -> Bool
verificaMovimentoTorre (Jogada posi posf) jo = lista /= [] && (posf `elem` lista)
    where lista = listaTodasJogadasTorre posi jo

listaTodasJogadasTorre :: Posicao -> Jogo -> [Posicao]
listaTodasJogadasTorre posi@(x1,y1) jo = possiveisCB ++ possiveisCC ++ possiveisCD ++ possiveisCE
    where
        corTorre = getCorPosicao posi jo

        xscima = if x1+1 /= 8 then [(x1+1)..7] else []              -- todas as linhas para cima da casa
        xsbaixo = if x1-1 /= -1 then reverse [0..(x1-1)] else []    -- todas as linhas para baixo da casa
        ysesquerda = if y1-1 /= -1 then reverse [0..(y1-1)] else [] -- todas as colunas a esquerda da casa
        ysdireita = if y1+1 /= 8 then [(y1+1)..7] else []           -- todas as colunas a direita da casa

        casasCima = map (\x -> (x, y1)) xscima                      -- todas as casas na mesma coluna para cima da casa
        casasBaixo = map (\x -> (x, y1)) xsbaixo                    -- todas as casas na mesma coluna para baixo da casa
        casasEsquerda = map (\y -> (x1, y)) ysesquerda              -- todas as casas na mesma linha para esquerda da casa
        casasDireita = map (\y -> (x1, y)) ysdireita                -- todas as casas na mesma linha para direita da casa

        possiveisCC = if null casasCima then [] else                                                                                                 -- todas as casas válidas na mesma coluna para cima da casa
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasCima in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corTorre]

        possiveisCB = if null casasBaixo then [] else                                                                                                 -- todas as casas válidas na mesma coluna para baixo da casa
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasBaixo in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corTorre]

        possiveisCE = if null casasEsquerda then [] else                                                                                                 -- todas as casas válidas na mesma linha para esquerda da casa
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasEsquerda in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corTorre]

        possiveisCD = if null casasDireita then [] else                                                                                                 -- todas as casas válidas na mesma linha para direita da casa
            let tuplares = span (\p -> pecaVazia == getPecaPosicao p jo) casasDireita in
                fst tuplares ++ [head (snd tuplares) | not (null (snd tuplares)) && getCorPosicao (head (snd tuplares)) jo == alternaCor corTorre]

-- Função que verifica se uma jogada feito com a dama para o momento do jogo atual é válida
verificaMovimentoDama :: Jogada -> Jogo -> Bool
verificaMovimentoDama (Jogada posi posf) jo = lista /= [] && (posf `elem` lista)
    where lista = listaTodasJogadasDama posi jo

listaTodasJogadasDama :: Posicao -> Jogo -> [Posicao]
listaTodasJogadasDama posi@(x1,y1) jo = listaTodasJogadasBispo posi jo ++ listaTodasJogadasTorre posi jo -- uma dama se move como a junção de um bispo com torre

-- Função que verifica se uma jogada feito com o cavalo para o momento do jogo atual é válida
verificaMovimentoCavalo :: Jogada -> Jogo -> Bool
verificaMovimentoCavalo (Jogada posi posf) jo = lista /= [] && (posf `elem` lista)
    where lista = listaTodasJogadasCavalo posi jo

listaTodasJogadasCavalo :: Posicao -> Jogo -> [Posicao]
listaTodasJogadasCavalo posi@(x1,y1) jo = possiveldid1 ++ possiveldid2 ++ possiveldie1 ++ possiveldie2 ++ possiveldsd1 ++ possiveldsd2 ++ possiveldse1 ++ possiveldse2
    where
        corCavalo = getCorPosicao posi jo

        dse1 = [(x1+2, y1-1) | y1 /= 0 && x1 /= 6 && x1 /= 7] -- todas as duas jogadas de cavalo possíveis para a diagonal superior esquerda
        dse2 = [(x1+1, y1-2) | y1 /= 0 && y1 /= 1 && x1 /= 7]

        dsd1 = [(x1+2, y1+1) | y1 /= 7 && x1 /= 6 && x1 /= 7] -- todas as duas jogadas de cavalo possíveis para a diagonal superior direita
        dsd2 = [(x1+1, y1+2) | y1 /= 6 && y1 /= 7 && x1 /= 7]

        die1 = [(x1-2, y1-1) | y1 /= 0 && x1 /= 1 && x1 /= 0] -- todas as duas jogadas de cavalo possíveis para a diagonal inferior esquerda
        die2 = [(x1-1, y1-2) | y1 /= 0 && y1 /= 1 && x1 /= 0]

        did1 = [(x1-2, y1+1) | y1 /= 7 && x1 /= 1 && x1 /= 0] -- todas as duas jogadas de cavalo possíveis para a diagonal inferior direita
        did2 = [(x1-1, y1+2) | y1 /= 6 && y1 /= 7 && x1 /= 0]

        possiveldse1
          | null dse1 = []
          | (getPecaPosicao (head dse1) jo == pecaVazia) || (getCorPosicao (head dse1) jo == alternaCor corCavalo) = dse1
          | otherwise = []

        possiveldse2
          | null dse2 = []
          | (getPecaPosicao (head dse2) jo == pecaVazia) || (getCorPosicao (head dse2) jo == alternaCor corCavalo) = dse2
          | otherwise = []

        possiveldsd1
          | null dsd1 = []
          | (getPecaPosicao (head dsd1) jo == pecaVazia) || (getCorPosicao (head dsd1) jo == alternaCor corCavalo) = dsd1
          | otherwise = []

        possiveldsd2
          | null dsd2 = []
          | (getPecaPosicao (head dsd2) jo == pecaVazia) || (getCorPosicao (head dsd2) jo == alternaCor corCavalo) = dsd2
          | otherwise = []

        possiveldie1
          | null die1 = []
          | (getPecaPosicao (head die1) jo == pecaVazia) || (getCorPosicao (head die1) jo == alternaCor corCavalo) = die1
          | otherwise = []

        possiveldie2
          | null die2 = []
          | (getPecaPosicao (head die2) jo == pecaVazia) || (getCorPosicao (head die2) jo == alternaCor corCavalo) = die2
          | otherwise = []

        possiveldid1
          | null did1 = []
          | (getPecaPosicao (head did1) jo == pecaVazia) || (getCorPosicao (head did1) jo == alternaCor corCavalo) = did1
          | otherwise = []

        possiveldid2
          | null did2 = []
          | (getPecaPosicao (head did2) jo == pecaVazia) || (getCorPosicao (head did2) jo == alternaCor corCavalo) = did2
          | otherwise = []

-- Função que verifica se uma jogada feito com o peão para o momento do jogo atual é válida
verificaMovimentoPeao :: Jogada -> Jogo -> Bool
verificaMovimentoPeao (Jogada posi posf) jo = lista /= [] && (posf `elem` lista)
    where lista = listaTodasJogadasPeao posi jo

listaTodasJogadasPeao :: Posicao -> Jogo -> [Posicao]
listaTodasJogadasPeao posi@(x1,y1) jo
  | corPeao == Branca =
    if x1 == 1 then
        pecasInicialB
    else if x1 == 7 then [] else
        pecasB
  | x1 == 6 = pecasInicialP
  | x1 == 0 = []
  | otherwise = pecasP
  where
    corPeao = getCorPosicao posi jo

    pecasInicialB = pecasB ++ [(x1 + 2, y1) | (getPecaPosicao (x1 + 1, y1) jo == pecaVazia) && (getPecaPosicao (x1 + 2, y1) jo == pecaVazia)]

    pecasB = [(x1 + 1, y1) | getPecaPosicao (x1 + 1, y1) jo == pecaVazia] ++
        [(x1 + 1, y1 + 1) | y1 /= 7 && (getCorPosicao (x1 + 1, y1 + 1) jo == alternaCor corPeao)] ++
        [(x1 + 1, y1 - 1) | y1 /= 0 && (getCorPosicao (x1 + 1, y1 - 1) jo == alternaCor corPeao)]

    pecasInicialP = pecasP ++ [(x1 - 2, y1) | (getPecaPosicao (x1 - 1, y1) jo == pecaVazia) && (getPecaPosicao (x1 - 2, y1) jo == pecaVazia)]

    pecasP = ([(x1 - 1, y1) | getPecaPosicao (x1 - 1, y1) jo == pecaVazia]) ++
        [(x1 - 1, y1 + 1) | y1 /= 7 && (getCorPosicao (x1 - 1, y1 + 1) jo == alternaCor corPeao)] ++
        [(x1 - 1, y1 - 1) | y1 /= 0 && (getCorPosicao (x1 - 1, y1 - 1) jo == alternaCor corPeao)]

verificaMovimentoRei :: Jogada -> Jogo -> Bool
verificaMovimentoRei (Jogada posi posf) jo = not (null lista) && (posf `elem` lista)
    where lista = listaTodasJogadasRei posi jo

listaTodasJogadasRei :: Posicao -> Jogo -> [Posicao] -- TODO: adicionar casos de roque - anda 2 casas
listaTodasJogadasRei posi@(x1,y1) jo
  | (x1 == 0) && (y1 /= 0) && (y1 /= 7) = casaCima ++ casaEsquerda ++ casaDireita ++ casaSD ++ casaSE
  | (x1 == 0) && (y1 == 0) = casaCima ++ casaDireita ++ casaSD
  | (x1 == 0) && (y1 == 7) = casaCima ++ casaEsquerda ++ casaSE
  | (x1 == 7) && (y1 /= 0) && (y1 /= 7) = casaBaixo ++ casaEsquerda ++ casaDireita ++ casaIE ++ casaID
  | (x1 == 7) && (y1 == 0) = casaBaixo ++ casaDireita ++ casaID
  | (x1 == 7) && (y1 == 7) = casaBaixo ++ casaEsquerda ++ casaIE
  | y1 == 0 = casaCima ++ casaBaixo ++ casaDireita ++ casaSD ++ casaID
  | y1 == 7 = casaCima ++ casaBaixo ++ casaEsquerda ++ casaSE ++ casaIE
  | otherwise = casaCima ++ casaBaixo ++ casaDireita ++ casaEsquerda ++ casaSE ++ casaSD ++ casaIE ++ casaID
  where
      corRei = getCorPosicao posi jo
      casaCima
        = [(x1 + 1, y1) |
             (getPecaPosicao (x1 + 1, y1) jo == pecaVazia)
               || (getCorPosicao (x1 + 1, y1) jo == alternaCor corRei)]
      casaBaixo
        = [(x1 - 1, y1) |
             (getPecaPosicao (x1 - 1, y1) jo == pecaVazia)
               || (getCorPosicao (x1 - 1, y1) jo == alternaCor corRei)]
      casaEsquerda
        = [(x1, y1 - 1) |
             (getPecaPosicao (x1, y1 - 1) jo == pecaVazia)
               || (getCorPosicao (x1, y1 - 1) jo == alternaCor corRei)]
      casaDireita
        = [(x1, y1 + 1) |
             (getPecaPosicao (x1, y1 + 1) jo == pecaVazia)
               || (getCorPosicao (x1, y1 + 1) jo == alternaCor corRei)]
      casaSE
        = [(x1 + 1, y1 - 1) |
             (getPecaPosicao (x1 + 1, y1 - 1) jo == pecaVazia)
               || (getCorPosicao (x1 + 1, y1 - 1) jo == alternaCor corRei)]
      casaSD
        = [(x1 + 1, y1 + 1) |
             (getPecaPosicao (x1 + 1, y1 + 1) jo == pecaVazia)
               || (getCorPosicao (x1 + 1, y1 + 1) jo == alternaCor corRei)]
      casaIE
        = [(x1 - 1, y1 - 1) |
             (getPecaPosicao (x1 - 1, y1 - 1) jo == pecaVazia)
               || (getCorPosicao (x1 - 1, y1 - 1) jo == alternaCor corRei)]
      casaID
        = [(x1 - 1, y1 + 1) |
             (getPecaPosicao (x1 - 1, y1 + 1) jo == pecaVazia)
               || (getCorPosicao (x1 - 1, y1 + 1) jo == alternaCor corRei)]