module Xadrez where

import Tipos

data Jogo = Jogo {  tabuleiro :: Tabuleiro, -- Lista 2D de pecas\posicoes do tabuleiro
                    vezDeQuem :: Cor, -- turno do jogo (branca ou preta a jogar)
                    --reiBranco :: Either Bool Bool,
                    --reiPreto :: Either Bool Bool,
                    reiBrancoMov :: Bool, -- indica se o rei branco ja se moveu da posição inicial
                    reiPretoMov :: Bool, -- indica se o rei preto ja se moveu da posição inicial
                    --torreBranca :: Either Bool Bool,
                    --torrePreta :: Either Bool Bool,
                    torreBrancaEMov :: Bool, -- indica se a torre branca esquerda ja se moveu da posição inicial
                    torreBrancaDMov :: Bool, -- indica se a torre branca direita ja se moveu da posição inicial
                    torrePretaEMov :: Bool, -- indica se a torre preta esquerda ja se moveu da posição inicial
                    torrePretaDMov :: Bool -- indica se a torre preta direita ja se moveu da posição inicial
                } deriving (Show, Read, Eq)

fazJogada :: Jogada -> Jogo -> Tabuleiro
fazJogada jog@(Jogada pi pf) jo = movePecaTabuleiro pi pf $ tabuleiro jo -- move a peça no tabuleiro para jogada dada

getPecaPosicao :: Posicao -> Jogo -> Peca -- retorna a peça da posição no jogo
getPecaPosicao pos jo = getPeca pos (tabuleiro jo)

getCorPosicao :: Posicao -> Jogo -> Cor  -- retorna a cor da peça da posição no jogo
getCorPosicao pos jo = cor (getPecaPosicao pos jo)

jogoInicial :: Jogo
jogoInicial = Jogo tabuleiroInicial Branca False False False False False False