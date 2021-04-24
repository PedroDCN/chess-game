module Tipos where

{--
-- Um tipo que possui Cor e Tipodepeca
-- Onde tipo de peca pode ser
-- peão, cavalo, bispo, torre, dama, rei
--}

type Tabuleiro = [[Quadrado]]

data Quadrado = Peca CorPeca TipoPeca | Vazio deriving (Show, Eq)

data CorPeca = Branco | Preto | SemCor deriving (Show, Eq)

data TipoPeca = Bispo | Rei | Cavalo | Peao | Dama | Torre | SemTipo deriving (Show, Eq)

data Jogador = Humano | Bot deriving (Show, Eq)

data EstadoJogo = EstadoJogo {
    tabuleiro           :: Tabuleiro, -- Lista 2D de Quadrados
    turno               :: Jogador,   -- Turno do Jogador (Humano) ou do Bot
    reiBranco           :: Int,       -- Posição do Rei Branco
    reiPreto            :: Int,       -- Posição do Rei Preto
    movimentoHabilitado :: Bool,      -- Verdade se o Jogador pode se mover
    pontoInicialSetado  :: Bool,      -- Verdade se houver um clique esquerdo na peça
    pontoInicial        :: Int        -- Índice do quadrado selecionado
} deriving (Show)

-- Notação para as peças
bPeao, bCavalo, bBispo, bDama, bRei, bTorre, pPeao, pCavalo, pBispo, pDama, pRei, pTorre :: Quadrado
bPeao = (Peca Branco Peao)
bCavalo = (Peca Branco Cavalo)
bBispo = (Peca Branco Bispo)
bDama = (Peca Branco Dama)
bRei = (Peca Branco Rei)
bTorre = (Peca Branco Torre)
pPeao = (Peca Preto Peao)
pCavalo = (Peca Preto Cavalo)
pBispo = (Peca Preto Bispo)
pDama = (Peca Preto Dama)
pRei = (Peca Preto Rei)
pTorre = (Peca Preto Torre)
