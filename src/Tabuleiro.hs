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

-- Obter a cor de um quadrado
getCorQuadrado  ::  Quadrado  ->  CorPeca
getCorQuadrado( Peca cp tp) = cp
getCorQuadrado ( Vazio )        =  SemCor

-- Obter tipo de quadrado
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
    
    
 {- Utilitários do Estado do Jogo -}

-- Obter o Tabuleiro
getTabuleiro  ::  EstadoJogo  ->  Tabuleiro
getTabuleiro ( EstadoJogo {
    tabuleiro = t, turno = _,
    reiBranco = _, reiPreto = _, pontoInicialSetado = _,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = _
    }) = t

-- Obter o Turno
getTurno  ::  EstadoJogo  ->  Jogador
getTurno ( EstadoJogo {
    tabuleiro = _, turno = turn,
    reiBranco = _, reiPreto = _, pontoInicialSetado = _,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = _
    }) = turn

-- Rei Branco ou Rei Preto
getReiPos  ::  EstadoJogo  ->  CorPeca  ->  Int
getReiPos estado cor
    | cor == Branco  = getReiBrancoPos estado
    |  otherwise     = getReiPretoPos estado

-- Obter a posição do Rei Branco
getReiBrancoPos  ::  EstadoJogo  ->  Int
getReiBrancoPos ( EstadoJogo {
    tabuleiro = _, turno = _,
    reiBranco = bRei, reiPreto = _, pontoInicialSetado = _,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = _
    }) = bRei

-- Obter a posição do Rei Preto
getReiPretoPos  ::  EstadoJogo  ->  Int
getReiPretoPos ( EstadoJogo {
    tabuleiro = _, turno = _,
    reiBranco = _, reiPreto = pRei, pontoInicialSetado = _,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = _
    }) = pRei

-- 'Obter o Ponto Inicial
getPontoInicial  ::  EstadoJogo  ->  Int
getPontoIncial ( EstadoJogo {
    tabuleiro = _, turno = _,
    reiBranco = _, reiPreto = _, pontoInicialSetado = _,
    pontoIncial = pi, pontosTabuleiro = _, movimentoHabilitado = _
    }) = pi

-- Obter os Pontos do Tabuleiro
getPontosTabuleiro  ::    -> [ QuadradoTabuleiro ]
getPontosTabuleiro ( EstadoJogo {
    tabuleiro = _, turno = _,
    reiBranco = _, reiPreto = _, pontoInicialSetado = _,
    pontoInicial= _, pontosTabuleiro = pt, movimentoHabilitado = _
    }) = pt

-- Obter o Movimento Habilitado
getMovimentoHabilitado  ::  EstadoJogo  ->  Bool
getMovimentoHabilitado ( EstadoJogo {
    tabuleiro = _, turno = _,
    reiBranco = _, reiPreto = _, pontoInicialSetado = _,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = mh
    }) = mh

-- Obter o Ponto Inicial Setado
ehPontoInicialSetado  ::  EstadoJogo  ->  Bool
ehPontoInicialSetado ( EstadoJogo {
   tabuleiro = _, turno = _,
    reiBranco = _, reiPreto = _, pontoInicialSetado = pis,
    pontoInicial = _, pontosTabuleiro = _, movimentoHabilitado = _
    }) = pis

-- Obter o quadrado do Tabuleiro em determinado índice (0-63)
getQuadradoAt  ::  EstadoJogo  ->  Int  ->  Quadrado
getQuadradoAt estado indice = ( \ tabuleiro linha coluna ->
        ((tabuleiro !! linha) !! coluna)
    ) (getTabuleiro estado) (índice `div`  8 ) (índice ` mod`  8 )

-- Obter a cor do quadrado do Tabuleiro em determinado índice (0-63)
getCorQuadradoAt  ::  EstadoJogo  ->  Int  ->  CorPeca
getCorQuadradoAt estado indice = ( \ tabuleiro linha coluna ->
        getCorQuadrado ((tabuleiro !! linha) !! coluna)
    ) (getTabuleiro) (índice `div`  8 ) (índice ` mod`  8 )


getPecaTabuleiroAt  ::  EstadoJogo  ->  Int  -> ( Int , GLfloat )
getPecaTabuleiroAt estado indice = ( \ (_, _, p) -> p) ((getPontosTabuleiro estado) !! indice)


