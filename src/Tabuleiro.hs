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
    pontoIncial = pti, pontosTabuleiro = _, movimentoHabilitado = _
    }) = pti

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
    ) (getTabuleiro estado) (índice `div`  8 ) (índice `mod`  8 )

-- Obter a cor do quadrado do Tabuleiro em determinado índice (0-63)
getCorQuadradoAt  ::  EstadoJogo  ->  Int  ->  CorPeca
getCorQuadradoAt estado indice = ( \ tabuleiro linha coluna ->
        getCorQuadrado ((tabuleiro !! linha) !! coluna)
    ) (getTabuleiro) (índice `div`  8 ) (índice `mod`  8 )


getPecaTabuleiroAt  ::  EstadoJogo  ->  Int  -> ( Int , GLfloat )
getPecaTabuleiroAt estado indice = ( \ (_, _, p) -> p) ((getPontosTabuleiro estado) !! indice)

-- Atualiza o quadrado em um índice
setQuadradoAt  ::  EstadoJogo  ->  Int  ->  Quadrado  ->  EstadoJogo
setQuadradoAt ( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt}) posicao quadrado =
    ( \ (r1, _ : r2) (c1, _ : c2) ->
        ( EstadoJogo {
            tabuleiro = (r1 ++ (c1 ++ (quadrado : c2)) : r2),
            turno = turn, movimentoHabilitado = mh,
            reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = pis,
            pontoInicial = pti, pontosTabuleiro = pt
        })
    ) ( SplitAt (pos `div` 8 ) tabuleiro) ( splitAt (pos `mod` 8 ) (tabuleiro !! (pos `div` 8 )))

-- Atualiza a posição do rei preto
setReiPretoPos  ::  EstadoJogo  ->  Int  ->  EstadoJogo
setReiPretoPos ( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt
    }) novaPosicao = ( EstadoJogo {
        tabuleiro = t, turno = turn, movimentoHabilitado = mh,
        reiBranco = bRei, reiPreto = novaPosicao, pontoIncialSetado = pis,
        pontoInicial = pti, pontosTabuleiro = pt
    })

-- Atualiza a posição do rei branco
setReiBrancoPos  ::  EsatdoJogo  ->  Int  ->  EstadoJogo
setReiBrancoPos ( EstadoJogo {
   tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt
    }) novaPosicao = ( EstadoJogo {
        tabuleiro = t, turno = turn, movimentoHabilitado = mh,
        reiBranco = novaPosicao, reiPreto = pRei, pontoIncialSetado = pis,
        pontoInicial = pti, pontosTabuleiro = pt
    })

-- Define se o ponto inicial é verdadeiro ou falso
setPontoIncialSetado  ::  GameState  ->  Bool  ->  GameState
setPontoIncialSetado ( GameState {
    tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt
    }) setPontSet = ( EstadoJogo {
        tabuleiro = t, turno = turn, movimentoHabilitado = mh,
        reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = setPontSet,
        pontoInicial = pti, pontosTabuleiro = pt
    })
    
-- Atualiza o ponto inical
setPontoIncial  ::  EstadoJogo  ->  Int  ->  EstadoJogo
setPontoInical ( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt
    }) novoPI = ( EstadoJogo {
        tabuleiro = t, turno = turn, movimentoHabilitado = mh,
        reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = pis,
        pontoInicial = novoPI, pontosTabuleiro = pt
    })

-- Atualiza os pontos do tabuleiro
setPontosTabuleiro  ::  EstadoJogo  -> [ QuadradoTabuleiro ] ->  EstadoJogo
setPontosTabuleiro ( EstadoJogo {
   tabuleiro = t, turno = turn, reiBranco = bRei,
    reiPreto = pRei, pontoInicialSetado = pis, movimentoHabilitado = mh,
    pontoInicial = pti, pontosTabuleiro = pt
    }) novoPT = ( EstadoJogo {
        tabuleiro = t, turno = turn, movimentoHabilitado = mh,
        reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = pis,
        pontoInicial = pti, pontosTabuleiro = novoPT
    })

-- Define a cor do ponto do tabuleiro em determinado índice
setPontoCorTabuleiroAt  ::  EstadoJogo  ->  Int  -> ( GLfloat , GLfloat , GLfloat ) ->  EstadoJogo
setPontoCorTabuleiroAt ( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei, movimentoHabilitado = mh,
    reiPreto = pRei, pontoIncialSetado = pis,
    pontoInicial = pti, pontosTabuleiro = pt}) indice novaCor =
    ( \ (l1, (coords, _, p) : l2) -> 
        ( EstadoJogo {
            tubalueiro = t, turno = turn, movimentoHabilitado = mh,
            reiBranco = bRei, reiPreto = pRei, pontoInicialSetado = pis,
            pontoInicial = pti, pontosTabuleiro = (l1 ++ [(coords, novaCor, p)] ++ l2)
        })
    ) $  splitAt indice pt

-- Define os pontos do tabuleiro em determinado índice
setPecaTabuleiroAt  ::  EstadoJogo  ->  Int  -> ( Int , GLfloat ) ->  EstadoJogo
setPecaTabuleiroAt ( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei, movimentoHabilitado = mh,
    reiPreto = pRei, pontoIncialSetado = pis,
    pontoIncial = pti, pontosTabuleiro = bp}) indice novaPeca =
    ( \ (l1, (coords, col, _) : l2) -> 
        ( EstadoJogo {
            tabuleiro = t, movimentoHabilitado = mh, turno = turn,
            reiBranco = bRei, reiPreto = pRei, pontoInicialSetado = pis,
            pontoInicial = pti, pontosTabuleiro = (l1 ++ [(coords, col, novaPeca)] ++ l2)
        })
    ) $  splitAt indice pt

-- Habilita movimento
habilitarMovimento  ::  EstadoJogo ->  EstadoJogo
habilitarMovimento( EstadoJogo {
    tabuleiro = t, turno = turn, reiBranco = bRei, movimentoHabilitado = _,
    reiPreto = pRei, pontoIncialSetado = pis,
    pontoInical = pti, pontosTabuleiro = pt})
    = ( EstadoJogo {
        tabueliro = t, turno = turn, movimentoHabilitado = True ,
        reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = pis,
        pontoInicial = pti, pontosTabuleiro = pt
    })

-- Desabilita movimento
desabilitarMovimento  ::  EstadoJogo  ->  EstadoJogo
desabilitarMovimento ( EstadoJogo {
     tabuleiro = t, turno = turn, reiBranco = bRei, movimentoHabilitado = _,
    reiPreto = pRei, pontoIncialSetado = pis,
    pontoInical = pti, pontosTabuleiro = pt})
    = ( EstadoJogo {
        tabueliro = t, turno = turn, movimentoHabilitado = False ,
        reiBranco = bRei, reiPreto = pRei, pontoIncialSetado = pis,
        pontoInicial = pti, pontosTabuleiro = pt
    })
