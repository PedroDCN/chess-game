module LogicaValidar where

import Tipos
import Tabuleiro
import Utilitarios
import Data.List

{--
-- Funções para verificação das jogadas (movimentos) feitas no jogo
-- se seguem as regras do xadrez.
--
-- função que verifica se um movimento é legal e retorna um boolean
-- (recebe o estado do tabuleiro atual e o movimento)
-- movimento legal:
-- - não pode colocar uma peça em cima de outra de mesma cor.
-- - peça só se movimenta seguindo as regras de movimento da mesma.
-- - não pode pular por cima de uma peça pra ir para a casa da frente\atrás dela (exceção do cavalo).
-- - quando em cheque, deve fazer uma jogada para sair do cheque, 
--   seja capturando a peça que da cheque, colocando uma peça na frente, ou movendo o rei
--   (caso especial de cheque duplo = deve morrer o rei => 
--   verificar depois da possível jogada se ainda vai estar em cheque para saber se é duplo),
--   se não for possível sair do cheque, é cheque mate.
-- - roque só é possível se as casas entre rei e torre não estão ameaçadas,
--   e o rei e a torre estão na posição inicial (manter variáveis globais pra
--   pra saber se o rei ou a torre se moveram).
-- - en passant só é possível se o peão inimigo acabar de ter sido movido
--   (verificar se a jogada anterior é a do peão)
-- - você não pode fazer uma jogada que deixe seu rei em cheque
--}

-- LÓGICA JOGAR 
listaMovimentosRei :: EstadoJogo -> Int -> [Int] -> [Int]
listaMovimentosRei estado _ [] = []
listaMovimentosRei estado inicio (x:xs) =
    if (inicio`div`8) == (x`div`8) && (abs ((inicio`mod`8) - (x`mod`8))) == 1 then [x] ++ (listaMovimentosRei estado inicio xs)
    else if (inicio`mod`8) == (x`mod`8) && (abs ((inicio`div`8) - (x`div`8))) == 1 then [x] ++ (listaMovimentosRei estado inicio xs)
    else if (abs ((inicio`div`8) - (x`div`8))) == 1 && (abs ((inicio`mod`8) - (x`mod`8))) == 1 then [x] ++ (listaMovimentosRei estado inicio xs)
    else (listaMovimentosRei estado inicio xs)

listaMovimentosDama :: EstadoJogo -> Int -> [Int] -> [Int]
listaMovimentosDama estado _ [] = []
listaMovimentosDama estado inicio (x:xs) =
    if (inicio /= x) && ((verificaMovimentoTorre estado inicio x) || (verificaMovimentoBispo estado inicio x)) then [x] ++ (listaMovimentosDama estado inicio xs)
    else (listaMovimentosDama estado inicio xs)

listaMovimentosTorre :: EstadoJogo -> Int -> Int -> Bool
listaMovimentosTorre estado inicio fim
    | (inicio == fim) = False
    | (colunaInicio == colunaFim) = ((inicio > fim)
        && ((foldr (&&) True (map (estaVazio estado) [(inicio - 8),(inicio - 16)..(fim + 8)]))))
        || ((not (inicio > fim))
        && (foldr (&&) True (map (estaVazio estado) [(inicio + 8),(inicio + 16)..(fim - 8)])))
    | (linhaInicio == linhaFim) = ((inicio > fim)
        && (foldr (&&) True (map (estaVazio estado) [(inicio - 1),(inicio - 2)..(fim + 1)])))
        || ((not (inicio > fim))
        && (foldr (&&) True (map (estaVazio estado) [(inicio + 1),(inicio + 2)..(fim - 1)])))
    | otherwise = False
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8

listaMovimentosCavalo :: EstadoJogo -> Int -> [Int] -> [Int]
listaMovimentosCavalo estado _ [] = []
listaMovimentosCavalo estado inicio (x:xs) =
    if (linhasMovidas /= 0) && (colunasMovidas /= 0) && ((linhasMovidas + colunasMovidas) == 3) then [x] ++ (listaMovimentosCavalo estado inicio xs)
    else (listaMovimentosCavalo estado inicio xs)
    where 
        linhasMovidas = (abs $ (inicio `div` 8) - (x `div` 8)) 
        colunasMovidas = (abs $ (inicio `mod` 8) - (x `mod` 8))

listaMovimentoBispo :: EstadoJogo -> Int -> Int -> Bool
listaMovimentoBispo estado inicio fim
    | (inicio == fim) || ((abs (linhaInicio - linhaFim)) /= (abs (colunaInicio - colunaFim))) = False
    | ((linhaInicio - linhaFim) == (colunaFim - colunaInicio)) =
        ((inicio > fim) && (foldr (&&) True (map (estaVazio estado) [(inicio - 7), (inicio - 14)..(fim + 7)])))
    ||  ((inicio < fim) && (foldr (&&) True (map (estaVazio estado) [(fim - 7), (fim - 14)..(inicio + 7)])))
    | otherwise =
        ((inicio > fim) && (foldr (&&) True (map (estaVazio estado) [(inicio - 9), (inicio - 18)..(fim + 9)])))
    ||  ((inicio < fim) && (foldr (&&) True (map (estaVazio estado) [(fim - 9), (fim - 18)..(inicio + 9)])))
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8

listaMovimentosPeao :: EstadoJogo -> Int -> Int -> CorPeca -> Bool
listaMovimentosPeao estado inicio fim cor
    | (peca == Vazio) || (linhaInicio == linhaFim) = False
    | (colunaInicio == colunaFim) = (getQuadradoAt estado fim) == Vazio
        && ((linhaInicio - linhaFim == 1)
            || (
                linhaInicio == 6
                && (linhaInicio - linhaFim) == 2
                && (getQuadradoAt estado (40 + colunaInicio)) == Vazio
            )
        )
    | otherwise = (
        (abs (colunaInicio - colunaFim)) == 1
        && (linhaInicio - linhaFim) == 1
        && (getCorQuadrado (getQuadradoAt estado fim)) == corOposta
    )
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8
        corOposta    = if cor == Branco then Preto else Branco
        peca         = getQuadradoAt estado inicio


-- FIM LÓGICA JOGAR


verificaMovimentoRei :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoRei estado inicio fim =
    (\ linhaInicio colunaInicio linhaFim colunaFim ->
        ((linhaInicio == linhaFim) && (abs (colunaInicio - colunaFim)) == 1)            
        || ((colunaInicio == colunaFim) && (abs (linhaInicio - linhaFim)) == 1)         
        || ((abs (linhaInicio - linhaFim)) == 1 && (abs (colunaInicio - colunaFim)) == 1)
    ) (inicio`div`8) (inicio`mod`8) (fim`div`8) (fim`mod`8)

verificaMovimentoTorre :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoTorre estado inicio fim
    | (inicio == fim) = False
    | (colunaInicio == colunaFim) = ((inicio > fim)
        && ((foldr (&&) True (map (estaVazio estado) [(inicio - 8),(inicio - 16)..(fim + 8)]))))
        || ((not (inicio > fim))
        && (foldr (&&) True (map (estaVazio estado) [(inicio + 8),(inicio + 16)..(fim - 8)])))
    | (linhaInicio == linhaFim) = ((inicio > fim)
        && (foldr (&&) True (map (estaVazio estado) [(inicio - 1),(inicio - 2)..(fim + 1)])))
        || ((not (inicio > fim))
        && (foldr (&&) True (map (estaVazio estado) [(inicio + 1),(inicio + 2)..(fim - 1)])))
    | otherwise = False
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8

verificaMovimentoCavalo :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoCavalo estado inicio fim =
    (\linhasMovidas colunasMovidas ->
        (linhasMovidas /= 0) && (colunasMovidas /= 0) && ((linhasMovidas + colunasMovidas) == 3)
    ) (abs $ (inicio `div` 8) - (fim `div` 8)) (abs $ (inicio `mod` 8) - (fim `mod` 8))

verificaMovimentoBispo :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoBispo estado inicio fim
    | (inicio == fim) || ((abs (linhaInicio - linhaFim)) /= (abs (colunaInicio - colunaFim))) = False
    | ((linhaInicio - linhaFim) == (colunaFim - colunaInicio)) =
        ((inicio > fim) && (foldr (&&) True (map (estaVazio estado) [(inicio - 7), (inicio - 14)..(fim + 7)])))
    ||  ((inicio < fim) && (foldr (&&) True (map (estaVazio estado) [(fim - 7), (fim - 14)..(inicio + 7)])))
    | otherwise =
        ((inicio > fim) && (foldr (&&) True (map (estaVazio estado) [(inicio - 9), (inicio - 18)..(fim + 9)])))
    ||  ((inicio < fim) && (foldr (&&) True (map (estaVazio estado) [(fim - 9), (fim - 18)..(inicio + 9)])))
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8

verificaMovimentoDama :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoDama estado inicio fim =
    (inicio /= fim) && 
    ((verificaMovimentoTorre estado inicio fim) || (verificaMovimentoBispo estado inicio fim))

verificaMovimentoPeao :: EstadoJogo -> Int -> Int -> CorPeca -> Bool
verificaMovimentoPeao estado inicio fim cor
    | (peca == Vazio) || (linhaInicio == linhaFim) = False
    | (colunaInicio == colunaFim) = (getQuadradoAt estado fim) == Vazio
        && ((linhaInicio - linhaFim == 1)
            || (
                linhaInicio == 6
                && (linhaInicio - linhaFim) == 2
                && (getQuadradoAt estado (40 + colunaInicio)) == Vazio
            )
        )
    | otherwise = (
        (abs (colunaInicio - colunaFim)) == 1
        && (linhaInicio - linhaFim) == 1
        && (getCorQuadrado (getQuadradoAt estado fim)) == corOposta
    )
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8
        corOposta    = if cor == Branco then Preto else Branco
        peca         = getQuadradoAt estado inicio

verificaMovimento :: EstadoJogo -> Int -> Int -> Bool
verificaMovimento estado inicio fim
    | not movimentoValido = False
    | otherwise = case tipoPeca of
        Peao      -> (verificaMovimentoPeao estado inicio fim corPeca)
        Cavalo    -> (verificaMovimentoCavalo estado inicio fim)
        Bispo     -> (verificaMovimentoBispo estado inicio fim)
        Rei       -> (verificaMovimentoRei estado inicio fim)
        Dama      -> (verificaMovimentoDama estado inicio fim)
        Torre     -> (verificaMovimentoTorre estado inicio fim)
        otherwise -> False
    where
        quadrado        = getQuadradoAt estado inicio
        tipoPeca        = getTipoQuadrado quadrado
        corPeca         = getCorQuadrado quadrado
        turno           = getTurno estado
        movimentoValido = (inicio >= 0 && inicio <= 63 && fim >= 0 && fim <= 63)
            && (
                ((turno == Humano) && (corPeca == Branco))
                || ((turno == Computador) && (corPeca == Preto))
               )
            && (corPeca /= (getCorQuadrado (getQuadradoAt estado fim)))

estaVazio :: EstadoJogo -> Int -> Bool
estaVazio estado indice = ((getQuadradoAt estado indice) == Vazio)

-- Verifica se é um xeque-mate (xeque, mate)
verificaXequeMate :: EstadoJogo -> CorPeca -> (Bool, Bool)
verificaXequeMate estado cor =
    (\quadradoRei ->
        (\(primeiraLista, segundaLista) ->
            (\listaXeque ->
                (
                    length (listaXeque) > 0, -- Verificação para o Xeque
                    (length(listaXeque) > 0) && (not (podeMoverRei estado cor quadradoRei)) -- Verificação para o Mate
                    && (not $ podeAtacarPecaXeque estado cor quadradoRei (primeiraLista, segundaLista) listaXeque)
                )
            ) $ filter (\valor -> valor >= 0) (primeiraLista ++ segundaLista)
        ) $ pegaPosicoesXeque estado (inverteCor cor) False quadradoRei
    ) $ getReiPos estado cor

-- Checa se é um Xeque
estaEmXeque :: EstadoJogo -> CorPeca -> Bool
estaEmXeque estado cor = verificaXeque estado cor False (getReiPos estado cor)

-- Computa a lista de todas as posições de xeque
verificaXeque :: EstadoJogo -> CorPeca -> Bool -> Int -> Bool
verificaXeque estado cor primeiraIteracao quadradoRei =
    let (primeiraLista, segundaLista) = pegaPosicoesXeque estado (inverteCor cor) primeiraIteracao quadradoRei in
    let listaXeque = filter(\valor -> valor >= 0) (primeiraLista ++ segundaLista) in (length(listaXeque) > 0)

-- Achar o quadrado pela linha e coluna
pegaQuadradoIndice :: (Int,Int) -> Int
pegaQuadradoIndice (x,y) = x * 8 + y

-- Verifica se o rei pode se mover
podeMoverRei :: EstadoJogo -> CorPeca -> Int -> Bool
podeMoverRei estado cor quadrado =
    let l = quadrado `div` 8 in -- l (linha)
    let c = quadrado `mod` 8 in -- c (coluna)
    let lista = filter (\valor -> (getCorQuadrado estado valor) /= cor) $ map pegaQuadradoIndice [(ll,cc) | ll <- [l-1, l+1], cc <- [c-1, c+1], ll >= 0, cc >= 0, ll <= 7, cc <= 7, (ll,cc) /= (l,c)] in
    not (foldr (&&) True (map (verificaXeque estado cor True) lista))

-- Tenta bloquear ou atacar a peça que está atacando o rei
podeAtacarPecaXeque :: EstadoJogo -> CorPeca -> Int -> ([Int],[Int]) -> [Int] -> Bool
podeAtacarPecaXeque estado cor quadrado (primeiraLista,segundaLista) listaXeque
    | (length(listaXeque) >= 2) = False -- Se tem mais de um Xeque acabou
    | (length(listaXeque) == 1) =       -- Se só possuir um xeque, então tenta atacar ou bloquear
    if (length(filter(\valor -> valor >= 0) (segundaLista)) == 1) then podeAtacarCavalo estado (inverteCor cor) (listaXeque !! 0)
    else podeBloquearPecaXeque estado (inverteCor cor) quadrado (primeiraLista,segundaLista) listaXeque
podeAtacarPecaXeque _ _ _ _ _ = True

podeAtacarCavalo :: EstadoJogo -> CorPeca -> Int -> Bool
podeAtacarCavalo estado cor quadrado = verificaXeque estado cor True quadrado

podeBloquearPecaXeque :: EstadoJogo -> CorPeca -> Int -> ([Int],[Int]) -> [Int] -> Bool
podeBloquearPecaXeque estado cor quadrado (primeiraLista, segundaLista) listaXeque -- Acha o caminho para bloquear a peça que ataca o rei
    | (indice == Just 0) = verificaAtaqueColunaEsquerda           estado cor linha (cabecaListaXeque `mod` 8) coluna
    | (indice == Just 1) = verificaAtaqueColunaDireita            estado cor linha (cabecaListaXeque `mod` 8) coluna
    | (indice == Just 2) = verificaAtaqueLinhaAbaixo              estado cor linha (cabecaListaXeque `div` 8) coluna
    | (indice == Just 3) = verificaAtaqueLinhaAcima               estado cor linha (cabecaListaXeque `div` 8) coluna
    | (indice == Just 4) = verificaAtaqueDiagonalSuperiorEsquerdo estado cor linha (cabecaListaXeque) coluna
    | (indice == Just 5) = verificaAtaqueDiagonalInferiorEsquerdo estado cor linha (cabecaListaXeque) coluna
    | (indice == Just 6) = verificaAtaqueDiagonalSuperiorDireito  estado cor linha (cabecaListaXeque) coluna
    | (indice == Just 7) = verificaAtaqueDiagonalInferiorDireito  estado cor linha (cabecaListaXeque) coluna
    | otherwise          = verificaAtaquePeao                     estado cor cabecaListaXeque
    where
        cabecaListaXeque = listaXeque !! 0
        indice = elemIndex (cabecaListaXeque) primeiraLista
        linha = quadrado `div` 8
        coluna = quadrado `mod` 8

-- Funções para verificar de onde o ataque vem para a função acima

verificaAtaquePeao :: EstadoJogo -> CorPeca -> Int -> Bool
verificaAtaquePeao estado cor quadrado = verificaXeque estado cor False quadrado

-- l = linha, c coluna
verificaAtaqueColunaEsquerda :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueColunaEsquerda estado cor linhaInicio colunaFim colunaInicio =
    let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio], cc <- [colunaInicio-1,colunaInicio-2..colunaFim]] in
        foldr (||) False (map (verificaXeque estado cor False) lista)

verificaAtaqueColunaDireita :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueColunaDireita estado cor linhaInicio colunaFim colunaInicio =
    let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio], cc <- [colunaInicio+1,colunaInicio+2..colunaFim]] in
        foldr (||) False (map (verificaXeque estado cor False) lista)

verificaAtaqueLinhaAbaixo :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueLinhaAbaixo estado cor linhaInicio linhaFim colunaInicio =
    let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio+1,linhaInicio+2..linhaFim], cc <- [colunaInicio]] in
        foldr (||) False (map (verificaXeque estado cor False) lista)

verificaAtaqueLinhaAcima :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueLinhaAcima estado cor linhaInicio linhaFim colunaInicio =
    let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio-1,linhaInicio-2..linhaFim], cc <- [colunaInicio]] in
        foldr (||) False (map (verificaXeque estado cor False) lista)

verificaAtaqueDiagonalSuperiorEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueDiagonalSuperiorEsquerdo estado cor linhaInicio fim colunaInicio =
    (\linhaFim colunaFim ->
        let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio-1,linhaInicio-2..linhaFim], cc <- [colunaInicio-1,colunaInicio-2..colunaFim], abs(ll-linhaInicio) == abs(cc-colunaInicio)] in
            foldr (||) False (map (verificaXeque estado cor False) lista)
    ) (fim `div` 8) (fim `mod` 8)

verificaAtaqueDiagonalInferiorEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueDiagonalInferiorEsquerdo estado cor linhaInicio fim colunaInicio =
    (\linhaFim colunaFim ->
        let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio+1,linhaInicio+2..linhaFim], cc <- [colunaInicio-1,colunaInicio-2..colunaFim], abs(ll-linhaInicio) == abs(cc-colunaInicio)] in
            foldr (||) False (map (verificaXeque estado cor False) lista)
    ) (fim `div` 8) (fim `mod` 8)

verificaAtaqueDiagonalSuperiorDireito :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueDiagonalSuperiorDireito estado cor linhaInicio fim colunaInicio =
    (\linhaFim colunaFim ->
        let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio-1,linhaInicio-2..linhaFim], cc <- [colunaInicio+1,colunaInicio+2..colunaFim], abs(ll-linhaInicio) == abs(cc-colunaInicio)] in
            foldr (||) False (map (verificaXeque estado cor False) lista)
    ) (fim `div` 8) (fim `mod` 8)

verificaAtaqueDiagonalInferiorDireito :: EstadoJogo -> CorPeca -> Int -> Int -> Int -> Bool
verificaAtaqueDiagonalInferiorDireito estado cor linhaInicio fim colunaInicio =
    (\linhaFim colunaFim ->
        let lista = map pegaQuadradoIndice [(ll,cc) | ll <- [linhaInicio+1,linhaInicio+2..linhaFim], cc <- [colunaInicio+1,colunaInicio+2..colunaFim], abs(ll-linhaInicio) == abs(cc-colunaInicio)] in
            foldr (||) False (map (verificaXeque estado cor False) lista)
    ) (fim `div` 8) (fim `mod` 8)

verificaPeaoXeque7 :: EstadoJogo -> CorPeca -> Int -> Int
verificaPeaoXeque7 estado cor celula
    | verificaAtaqueCimaAbaixo
        && (((celula-7) >= 0) && (getQuadradoAt estado (celula-7)) == (Peca cor Peao)) && (getQuadradoAt estado (celula))/=Vazio
        = celula-7
    | (not verificaAtaqueCimaAbaixo)
        && (((celula+7) <= 63) && (getQuadradoAt estado (celula+7)) == (Peca cor Peao)) && (getQuadradoAt estado (celula))/=Vazio
        = celula+7
    | otherwise = -1
    where
        minhaCor = if (getTurno estado)==JogadorBranco then Branco else Preto
        verificaAtaqueCimaAbaixo = (minhaCor/=cor)

verificaPeaoXeque9 :: EstadoJogo -> CorPeca -> Int -> Int
verificaPeaoXeque9 estado cor celula
    | verificaAtaqueCimaAbaixo
        && (((celula-9) >= 0) && (getQuadradoAt estado (celula-9)) == (Peca cor Peao)) && (getQuadradoAt estado (celula))/=Vazio
        = celula-9
    | (not verificaAtaqueCimaAbaixo)
        && (((celula+9) <= 63) && (getQuadradoAt estado (celula+9)) == (Peca cor Peao)) && (getQuadradoAt estado (celula))/=Vazio
        = celula+9
    | otherwise = -1
    where
        minhaCor = if (getTurno estado)==JogadorBranco then Branco else Preto
        verificaAtaqueCimaAbaixo = (minhaCor/=cor)

verificaXequeColunaEsquerda :: EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaXequeColunaEsquerda estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    | (getTipoQuadrado(getQuadradoAt estado celula)) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 0 then -1
        else verificaXequeColunaEsquerda estado cor False (celula-1)
    | (primeiraIteracao && celula `mod` 8 /= 0) = verificaXequeColunaEsquerda estado cor False (celula-1)
    | (celula >= 0) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 0 then verificaXequeColunaEsquerda estado cor False (celula-1)
             else -1
        else
             if ((getTipoQuadrado (getQuadradoAt estado celula)) == Torre || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama) then celula
             else -1
    | otherwise = -1

verificaXequeColunaDireita :: EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaXequeColunaDireita estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    | (getTipoQuadrado(getQuadradoAt estado celula)) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 7 then -1
        else verificaXequeColunaDireita estado cor False (celula+1)
    | (primeiraIteracao && celula `mod` 8 /= 7) = verificaXequeColunaDireita estado cor False (celula+1)
    | (celula <= 63) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 7 then verificaXequeColunaDireita estado cor False (celula+1)
             else -1
        else
             if ((getTipoQuadrado (getQuadradoAt estado celula)) == Torre || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama) then celula
             else -1
    | otherwise = -1

verificaXequeLinhaAbaixo ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaXequeLinhaAbaixo estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    | (getTipoQuadrado(getQuadradoAt estado celula)) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `div` 8 == 7 then -1
        else verificaXequeLinhaAbaixo estado cor False (celula+8)
    | (primeiraIteracao) = verificaXequeLinhaAbaixo estado cor False (celula+8)
    | (celula `div` 8) < 8 =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if getCorQuadrado (getQuadradoAt estado celula) == SemCor then
             if (celula `div` 8) /= 7 then verificaXequeLinhaAbaixo estado cor False (celula+8)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula)) == Torre || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaXequeLinhaAcima ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaXequeLinhaAcima estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    |  getTipoQuadrado(getQuadradoAt estado celula) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `div` 8 == 0 then -1
        else verificaXequeLinhaAcima estado cor False (celula-8)
    | (primeiraIteracao) = verificaXequeLinhaAcima estado cor False (celula-8)
    | ((celula `div` 8) < 8) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if getCorQuadrado (getQuadradoAt estado celula) == SemCor then
             if (celula `div` 8) /= 0 then verificaXequeLinhaAcima estado cor False (celula-8)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula)) == Torre || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaDiagonalSuperiorEsquerda ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaDiagonalSuperiorEsquerda estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    |  getTipoQuadrado(getQuadradoAt estado celula) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 0 || celula `div` 8 == 0 then -1
        else verificaDiagonalSuperiorEsquerda estado cor False (celula-9)
    | (primeiraIteracao && (celula `mod` 8) /= 0 && celula `div` 8 /= 0) = verificaDiagonalSuperiorEsquerda estado cor False (celula-9)
    | ((celula `div` 8) < 8) && ((celula `div` 8) >= 0) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 0 && celula `div` 8 /= 0 then verificaDiagonalSuperiorEsquerda estado cor False (celula-9)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula)) == Bispo || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaDiagonalInferiorEsquerda ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaDiagonalInferiorEsquerda estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    |  getTipoQuadrado(getQuadradoAt estado celula) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 0 || celula `div` 8 == 7 then -1
        else verificaDiagonalInferiorEsquerda estado cor False (celula+7)
    | (primeiraIteracao && (celula `mod` 8) /= 0 && celula `div` 8 /= 7) = verificaDiagonalInferiorEsquerda estado cor False (celula+7)
    | ((celula `div` 8) < 8) && ((celula `div` 8) >= 0) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 0 && celula `div` 8 /= 7 then verificaDiagonalInferiorEsquerda estado cor False (celula+7)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula) == Bispo) || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaDiagonalSuperiorDireita ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaDiagonalSuperiorDireita estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    |  getTipoQuadrado(getQuadradoAt estado celula) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 7 || celula `div` 8 == 0 then -1
        else verificaDiagonalSuperiorDireita estado cor False (celula-7)
    | (primeiraIteracao && (celula `mod` 8) /= 7 && celula `div` 8 /= 0) = verificaDiagonalSuperiorDireita estado cor False (celula-7)
    | ((celula `div` 8) < 8) && ((celula `div` 8) >= 0) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 7 && celula `div` 8 /= 0 then verificaDiagonalSuperiorDireita estado cor False (celula-7)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula) == Bispo) || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaDiagonalInferiorDireita ::  EstadoJogo -> CorPeca -> Bool -> Int -> Int
verificaDiagonalInferiorDireita estado cor primeiraIteracao celula
    | celula<0 || celula>63 = -1
    |  getTipoQuadrado(getQuadradoAt estado celula) == Rei && getCorQuadrado(getQuadradoAt estado celula) == inverteCor cor =
        if celula `mod` 8 == 7 || celula `div` 8 == 7 then -1
        else verificaDiagonalInferiorDireita estado cor False (celula+9)
    | (primeiraIteracao && (celula `mod` 8) /= 7 && celula `div` 8 /= 7) = verificaDiagonalInferiorDireita estado cor False (celula+9)
    | ((celula `div` 8) < 8) && ((celula `div` 8) >= 0) =
        if getCorQuadrado (getQuadradoAt estado celula) == inverteCor cor then -1
        else if (getCorQuadrado (getQuadradoAt estado celula) == SemCor) then
             if (celula `mod` 8) /= 7 && celula `div` 8 /= 7 then verificaDiagonalInferiorDireita estado cor False (celula+9)
             else -1
        else
             if (getTipoQuadrado (getQuadradoAt estado celula) == Bispo) || (getTipoQuadrado (getQuadradoAt estado celula)) == Dama then celula
             else -1
    | otherwise = -1

verificaXequeCavaloSuperiorEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloSuperiorEsquerdo estado cor celula
    | celula<0 || celula>63 = -1
    | (((celula-16) `div` 8)  >=0 && ((celula `mod` 8)-1) >= 0) =
           if getCorQuadrado (getQuadradoAt estado (celula -17)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula-17))) == Cavalo then (celula-17)
           else -1
    | otherwise = -1

verificaXequeCavaloSuperiorCentralEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloSuperiorCentralEsquerdo estado cor celula
    | celula<0 || celula>63 = -1
    | (((celula-8) `div` 8) >=0 && ((celula `mod` 8)-2) >=0) =
           if getCorQuadrado (getQuadradoAt estado (celula-10)) == cor && getTipoQuadrado (getQuadradoAt estado (celula-10)) == Cavalo then (celula-10)
           else -1
    | otherwise = -1

verificaXequeCavaloInferiorCentralEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int
checkLMidLeftHorseCheck estado cor celula
    | celula<0 || celula>63 = -1
    | (((celula `mod` 8) - 2) >=0 && (celula+8) `div` 8 <=7) =
           if getCorQuadrado (getQuadradoAt estado (celula+6)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula+6))) == Cavalo then (celula+6)
           else -1
    | otherwise = -1

verificaXequeCavaloInferiorEsquerdo :: EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloInferiorEsquerdo estado cor celula
    | celula<0 || celula>63 = -1
    | (((celula+16) `div` 8) <=7 && ((celula `mod` 8)-1) >=0) =
           if getCorQuadrado (getQuadradoAt estado (celula+15)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula+15))) == Cavalo then (celula+15)
           else -1
    | otherwise = -1

verificaXequeCavaloSuperiorDireito ::  EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloSuperiorDireito estado cor celula
    | celula<0 || celula>63 = -1
    | ((celula-16) `div` 8 >=0 && ((celula `mod` 8)+1)<= 7) =
          if getCorQuadrado (getQuadradoAt estado (celula -15)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula-15))) == Cavalo then (celula-15)
          else -1
    | otherwise = -1

verificaXequeCavaloSuperiorCentralDireito ::  EstadoJogo -> CorPeca -> Int -> Int
checkUMidRightHorseCheck estado cor celula
    | celula<0 || celula>63 = -1
    | ((celula-8) `div` 8 >=0 && ((celula `mod` 8)+2) <= 7) =
           if getCorQuadrado (getQuadradoAt estado (celula-6)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula-6))) == Cavalo then (celula-6)
           else -1
    | otherwise = -1

verificaXequeCavaloInferiorCentralDireito ::  EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloInferiorCentralDireito estado cor celula
    | celula<0 || celula>63 = -1
    | (((celula `mod` 8)+2) >=0 && (celula+8) `div` 8 <=7) =
            if getCorQuadrado (getQuadradoAt estado (celula+10)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula+10))) == Cavalo then (celula+10)
            else -1
    | otherwise = -1

verificaXequeCavaloInferiorDireito :: EstadoJogo -> CorPeca -> Int -> Int
verificaXequeCavaloInferiorDireito estado cor celula
    | celula<0 || celula>63 = -1
    | ((celula+16) `div` 8 <=7 && ((celula `mod` 8)+1) <=7) =
            if getCorQuadrado (getQuadradoAt estado (celula+17)) == cor && (getTipoQuadrado (getQuadradoAt estado (celula+17))) == Cavalo then (celula+17)
            else -1
    | otherwise = -1