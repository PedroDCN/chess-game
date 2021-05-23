{-# LANGUAGE TupleSections #-}
module LogicaXeque where

import Tipos
import Xadrez
import LogicaValidar
import LogicaJogar

-- retorna todas as jogadas possíveis para a cor da vez em um jogo
listaTodasJogadasPossiveis :: Jogo -> [Posicao]
listaTodasJogadasPossiveis jogo = concatMap (\pos ->
     if getCorPosicao pos jogo == vezDeQuem jogo then listaTodasJogadas pos jogo
    else [] ) listaTodasPosicoes

verificaPosicaoAfogado :: Jogo -> Bool
verificaPosicaoAfogado jogo = null (listaTodasJogadasPossiveis jogo)

verificaEstaEmCheque :: Jogo -> Bool
verificaEstaEmCheque jogo = pecasAtacandoDiagonal + pecasAtacandoHorVer + pecasAtacandoCavalo > 0
    where
        cor = vezDeQuem jogo
        posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos jogo) == Rei) &&
                        (getCorPosicao pos jogo == cor) then
                            pos:acc else acc) [] listaTodasPosicoes
        posRei = head posRei'

        listaPosicoesDiagonal = listaTodasJogadasBispo posRei jogo
        listaPosicoesHorizontalVertical = listaTodasJogadasTorre posRei jogo
        listaPosicoesCavalo = listaTodasJogadasCavalo posRei jogo

        listaPossiveisAtaquesDiagonal = foldl -- reduz a lista a apenas posições de peças inimigas
                                (\acc pos -> if getPecaPosicao pos jogo == pecaVazia
                                    then acc else pos : acc
                                    ) [] listaPosicoesDiagonal

        pecasAtacandoDiagonal = foldl
                            (\acc pos -> if tipo (getPecaPosicao pos jogo) == Bispo
                                || tipo (getPecaPosicao pos jogo) == Dama || (
                                    tipo (getPecaPosicao pos jogo) == Peao && (
                                        if cor == Branca then fst pos == fst posRei + 1 else fst pos == fst posRei - 1)
                                ) then acc+1 else acc
                            ) 0 listaPossiveisAtaquesDiagonal

        listaPossiveisAtaquesHorVer = foldl -- reduz a lista a apenas posições de peças inimigas
                                (\acc pos -> if getPecaPosicao pos jogo == pecaVazia
                                    then acc else pos : acc
                                    ) [] listaPosicoesHorizontalVertical

        pecasAtacandoHorVer = foldl
                            (\acc pos -> if tipo (getPecaPosicao pos jogo) == Torre
                                || tipo (getPecaPosicao pos jogo) == Dama then acc+1 else acc
                            ) 0 listaPossiveisAtaquesHorVer

        listaPossiveisAtaquesCavalo = foldl -- reduz a lista a apenas posições de peças inimigas
                                (\acc pos -> if getPecaPosicao pos jogo == pecaVazia
                                    then acc else pos : acc
                                    ) [] listaPosicoesCavalo

        pecasAtacandoCavalo = foldl
                            (\acc pos -> if tipo (getPecaPosicao pos jogo) == Cavalo
                                then acc+1 else acc
                            ) 0 listaPossiveisAtaquesCavalo

verificaEstaEmChequeMate :: Jogo -> Bool
verificaEstaEmChequeMate jogo = not $ or listaJogadasPossiveisNaoCheque
    where
        cor = vezDeQuem jogo
        -- listaTodasPosicoes = concatMap (\x -> map (x,) [0..7]) [0..7] :: [Posicao]
        posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos jogo) == Rei) &&
                        (getCorPosicao pos jogo == cor) then
                            pos:acc else acc) [] listaTodasPosicoes
        posRei = head posRei'

        listaJogadasPossiveisNaoCheque =  map (\pos ->
            if getCorPosicao pos jogo == vezDeQuem jogo then
                if null (foldl (\acc posf -> 
                    if verificaEstaEmCheque (mudaVez (fazerJogada (Jogada pos posf) jogo))
                        then acc else True:acc
                    )
                    [] (listaTodasJogadas pos jogo)) then False else True
            else False ) listaTodasPosicoes

-- para uma posição no jogo, lista todas as posições possíveis para ir de
-- daquela peça naquela posição
-- função interna
listaTodasJogadas :: Posicao -> Jogo -> [Posicao]
listaTodasJogadas pos jogo
  | tipo (getPecaPosicao pos jogo) == Rei = listaTodasJogadasRei pos jogo
  | tipo (getPecaPosicao pos jogo) == Dama = listaTodasJogadasDama pos jogo
  | tipo (getPecaPosicao pos jogo) == Torre = listaTodasJogadasTorre pos jogo
  | tipo (getPecaPosicao pos jogo) == Bispo = listaTodasJogadasBispo pos jogo
  | tipo (getPecaPosicao pos jogo) == Cavalo = listaTodasJogadasCavalo pos jogo
  | tipo (getPecaPosicao pos jogo) == Peao = listaTodasJogadasPeao pos jogo
  | otherwise = []

listaTodasPosicoes :: [Posicao]
listaTodasPosicoes = concatMap (\x -> map (x,) [0..7]) [0..7] :: [Posicao]