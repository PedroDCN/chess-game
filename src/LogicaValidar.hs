module LogicaValidar where

import Tipos
import Tabuleiro
import Utilitarios

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
verificaXequeMate :: EstadoJogo -> PecaCor -> (Bool, Bool)
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
    ) $ pegaPosicaoRei estado Cor