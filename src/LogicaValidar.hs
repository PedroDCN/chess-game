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

-- pegaQuadrado (retorna uma peça que está no quadrado ou Vazio)
-- pegaQuadradoCor (retorna a cor da peça no quadrado passado pro parâmetro)
verificaMovimentoPeao :: EstadoJogo -> Int -> Int -> CorPeca -> Bool
verificaMovimentoPeao estado inicio fim cor
    | (peca == Vazio) || (linhaInicio == linhaFim) = False
    | (colunaInicio == colunaFim) = (pegaQuadrado estado fim) == Vazio
        && ((linhaInicio - linhaFim == 1)
            || (
                linhaInicio == 6
                && (linhaInicio - linhaFim) == 2
                && (pegaQuadrado estado (40 + colunaInicio)) == Vazio
            )
        )
    | otherwise = (
        (abs (colunaInicio - colunaFim)) == 1
        && (linhaInicio - linhaFim) == 1
        && (pegaQuadradoCor (pegaQuadrado estado fim)) == corOposta
    )
    where
        linhaInicio  = inicio `div` 8
        linhaFim     = fim    `div` 8
        colunaInicio = inicio `mod` 8
        colunaFim    = fim    `mod` 8
        corOposta    = if cor == Branco then Preto else Branco
        peca         = pegaQuadrado estado inicio

-- Precisamos criar as funções do tabuleiro para pegar informações da peça (Cor e Tipo) que está no quadrado
verificaMovimento :: EstadoJogo -> Int -> Int -> Bool
verificaMovimento estado inicio fim
    | not movimentoValido = False
    | otherwise = case tipoPeca of
        Peao      -> (verificaMovimentoPeao estado inicio fim)
        Cavalo    -> (verificaMovimentoCavalo estado inicio fim)
        Bispo     -> (verificaMovimentoBispo estado inicio fim)
        Rei       -> (verificaMovimentoRei estado inicio fim)
        Dama      -> (verificaMovimentoDama estado inicio fim)
        Torre     -> (verificaMovimentoTorre estado inicio fim)
        otherwise -> False
    where
        quadrado        = (pegaQuadrado estado inicio)
        tipoPeca        = pegaQuadradoTipo quadrado
        corPeca         = pegaQuadradoCor quadrado
        turno           = pegaTurno estado
        movimentoValido = (inicio >= 0 && inicio <= 63 && fim >= 0 && fim <= 63)
            && (
                ((turno == Humano) && (corPeca == Branco))
                || ((turno == Computador) && (corPeca == Preto))
               )
            && (corPeca /= (pegaQuadradoCor (pegaQuadrado estado fim)))

-- Necessita de implementação em outra classe (método de pegar o quadrado no tabuleiro)
estaVazio :: EstadoJogo -> Int -> Bool
estaVazio estado indice = ((pegaQuadrado estado indice) == Vazio)

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
    ) $ pegaPosicaoRei estado cor

-- Checa se é um Xeque
estaEmXeque :: EstadoJogo -> CorPeca -> Bool
estaEmXeque estado cor = verificaXeque estado cor False (pegaPosicaoRei estado cor)

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
    let lista = filter (\valor -> (pegaQuadradoCor estado valor) /= cor) $ map pegaQuadradoIndice [(ll,cc) | ll <- [l-1, l+1], cc <- [c-1, c+1], ll >= 0, cc >= 0, ll <= 7, cc <= 7, (ll,cc) /= (l,c)] in
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