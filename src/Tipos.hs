module Tipos where

import qualified Data.List as L

data TipoPeca = Nada | Peao | Cavalo | Bispo | Torre | Dama | Rei
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Cor = Sem | Preta | Branca deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Posicao = (Int, Int)

type Tabuleiro = [[Peca]]

-- tipo jogada para definir o tipo de jogada feita: normal ou especial, e qual especial
data Jogada = Jogada {
                posini :: Posicao,
                posfin :: Posicao
            } | EspRockCurtoP {
                posini :: Posicao,
                posfin :: Posicao
            } | EspRockCurtoB {
                posini :: Posicao,
                posfin :: Posicao
            } | EspRockLongoP {
                posini :: Posicao,
                posfin :: Posicao
            } | EspRockLongoB {
                posini :: Posicao,
                posfin :: Posicao
            } | EnPassant {
                posini :: Posicao,
                posfin :: Posicao
            } | Promocao {
                posini :: Posicao,
                posfin :: Posicao
            } deriving (Eq, Show, Read)

data Peca = Peca {  tipo :: TipoPeca,
                    cor :: Cor
            } deriving (Eq, Ord, Show, Read)

alternaCor :: Cor -> Cor -- função para alternar a cor dada
alternaCor Preta = Branca
alternaCor Branca = Preta
alternaCor Sem = Sem

-- Cria tabuleiro quadrado genérico NxN como lista de listas de casas vazias
tabPadrao' :: Int -> Tabuleiro
tabPadrao' x = L.genericReplicate x $ L.genericReplicate x pecaVazia

-- Função que retorna o TipoPeca que está guardado na posição (x, y) do tabuleiro
-- é garantido que não irá se pegar posições fora do range (0, 7) do tamanho do tabuleiro
-- x representa a linha contada de baixo pra cima[1=0, 2=1, 3=2..], y a coluna da esquerda pra direita [a=0, b=1, c=2..]
getPeca :: Posicao -> Tabuleiro -> Peca
getPeca (x, y) tabuleiro = tabuleiro !! (7-x) !! y

-- Função que adiciona um noto TipoPeca numa posição (x, y) do tabuleiro e retorna o novo tabuleiro
-- é garantido que não irá se pegar posições fora do range (0, 7) do tamanho do tabuleiro
setPeca :: Posicao -> Peca -> Tabuleiro ->  Tabuleiro
setPeca (x, y) peca = tab
    where
        tab t = take (7-x) t ++ [linha (t!!(7-x))] ++ drop (7-x+1) t
        linha l = take y l ++ [peca] ++ drop (y+1) l

-- Função que remove uma peça da posição (x, y) do tabuleiro e retorna o novo tabuleiro formado
-- é garantdio que não irá se pegar uma posição forad o range (0, 7) do tamanho do tabuleiro
-- Na prática, remover uma peça de uma posição é adicionar uma peça vazia naquele lugar
removePeca :: Posicao -> Tabuleiro -> Tabuleiro
removePeca pos = setPeca pos pecaVazia

-- Função que move uma peça de pi para pf no tabuleiro
-- Função que não realiza verificação, é a função 'final' que realmente faz o movimento
-- Retorna um novo tabuleiro com a nova configuração do tabulerio para ser setado dentro do jogo
movePecaTabuleiro :: Posicao -> Posicao -> Tabuleiro -> Tabuleiro
movePecaTabuleiro pi pf tab = let p = getPeca pi tab
                               in let ntab = removePeca pi tab
                               in setPeca pf p ntab

-- Cria tabuleiro 8x8 como lista de listas de casas vazias
tabVazio :: Tabuleiro
tabVazio = tabPadrao' 8
tabuleiroInicial = linhaPreta $ linhaPeaoPreta $ linhaPeaoBranca $ linhaBranca tabVazio

linhaPreta tab = setPeca (7, 7) torreP $ setPeca (7, 6) cavaloP $ setPeca (7, 5) bispoP $ setPeca (7, 4) reiP $ 
            setPeca (7,3) damaP $ setPeca (7, 2) bispoP $ setPeca (7, 1) cavaloP $ setPeca (7, 0) torreP tab
linhaPeaoPreta tab = setPeca (6, 0) peaoP $ setPeca (6, 1) peaoP $ setPeca (6, 2) peaoP $ setPeca (6, 3) peaoP $ setPeca (6, 4) peaoP $
                setPeca (6, 5) peaoP $ setPeca (6, 6) peaoP $ setPeca (6, 7) peaoP tab

linhaPeaoBranca tab = setPeca (1, 0) peaoB $ setPeca (1, 1) peaoB $ setPeca (1, 2) peaoB $ setPeca (1, 3) peaoB $ setPeca (1, 4) peaoB $
                setPeca (1, 5) peaoB $ setPeca (1, 6) peaoB $ setPeca (1, 7) peaoB tab
linhaBranca tab = setPeca (0, 7) torreB $ setPeca (0, 6) cavaloB $ setPeca (0, 5) bispoB $ setPeca (0, 4) reiB $ 
            setPeca (0,3) damaB $ setPeca (0, 2) bispoB $ setPeca (0, 1) cavaloB $ setPeca (0, 0) torreB tab

pecaVazia, peaoB, peaoP, bispoB, bispoP, cavaloB, cavaloP, torreB, torreP, damaB, damaP, reiB, reiP :: Peca
pecaVazia = Peca Nada Sem
peaoB = Peca Peao Branca
peaoP = Peca Peao Preta
bispoB = Peca Bispo Branca
bispoP = Peca Bispo Preta
cavaloB = Peca Cavalo Branca
cavaloP = Peca Cavalo Preta
torreB = Peca Torre Branca
torreP = Peca Torre Preta
damaB = Peca Dama Branca
damaP = Peca Dama Preta
reiB = Peca Rei Branca
reiP = Peca Rei Preta