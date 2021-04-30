module Utilitarios where

import Tipos

-- Métodos para auxilixar o desenvolvimento da GUI

-- reset o EstadoJogo para um dado estado
resetaEstado :: EstadoJogo -> EstadoJogo -> EstadoJogo
resetaEstado anterior _ = anterior

inverteCor :: CorPeca -> CorPeca
inverteCor cor
    | cor == Branco = Preto
    | cor == Preto  = Branco
    | otherwise     = SemCor

linhaVazia :: [Quadrado]
linhaVazia = [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]

estadoInicialPreto :: Tabuleiro
estadoInicialPreto = [
        [bTorre, bCavalo, bBispo, bRei, bDama, bBispo, bCavalo, bTorre],
        [bPeao, bPeao, bPeao, bPeao, bPeao, bPeao, bPeao, bPeao],
        linhaVazia, linhaVazia, linhaVazia, linhaVazia,
        [pPeao, pPeao, pPeao, pPeao, pPeao, pPeao, pPeao, pPeao],
        [pTorre, pCavalo, pBispo, pRei, pDama, pBispo, pCavalo, pTorre]
    ]

estadoInicialBranco :: Tabuleiro
estadoInicialBranco = [
        [pTorre, pCavalo, pBispo, pDama, pRei, pBispo, pCavalo, pTorre],
        [pPeao, pPeao, pPeao, pPeao, pPeao, pPeao, pPeao, pPeao],
        linhaVazia, linhaVazia, linhaVazia, linhaVazia,
        [bPeao, bPeao, bPeao, bPeao, bPeao, bPeao, bPeao, bPeao],
        [bTorre, bCavalo, bBispo, bDama, bRei, bBispo, bCavalo, bTorre]
    ]

estadoJogoInicialBranco :: EstadoJogo
estadoJogoInicialBranco = EstadoJogo {
    tabuleiro = estadoInicialBranco,
    turno     = Humano,
    reiBranco = 60,
    reiPreto  = 4,
    pontoInicialSetado = False,
    pontoInicial = 0,
    movimentoHabilitado = True
}

estadoJogoInicialPreto :: EstadoJogo
estadoJogoInicialPreto = EstadoJogo {
    tabuleiro = estadoInicialPreto,
    turno     = Computador,
    reiBranco = 3,
    reiPreto  = 59,
    pontoInicialSetado = False,
    pontoInicial = 0,
    movimentoHabilitado = True
}