module LogicaValidar where

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

verificaMovimentoRei :: Int -> Int -> Bool
verificaMovimentoRei inicio fim =
    (\ linhaInicio colunaInicio linhaFim colunaFim ->
        ((linhaInicio == linhaFim) && (abs (colunaInicio - colunaFim))==1)            
        || ((colunaInicio == colunaFim) && (abs (linhaInicio - linhaFim))==1)         
        || ((abs (linhaInicio - linhaFim))==1 && (abs (colunaInicio - colunaFim))==1)
    ) (inicio`div`8) (inicio`mod`8) (fim`div`8) (fim`mod`8)

verificaMovimentoTorre :: EstadoJogo -> Int -> Int -> Bool
verificaMovimentoTorre estado casaInicio casaFim
    | (casaInicio == casaFim) = False
    | (colunaInicio == colunaFim) = ((casaInicio > casaFim)          
                                && ((foldr (&&) True  (map (estaVazio state) [(casaInicio - 8),(casaInicio - 16)..(casaFim + 8)]))))
                             || ((not (casaInicio > endCell)) 
                                && (foldr (&&) True  (map (estaVazio state) [(casaInicio + 8),(startCell+16)..(casaFim - 8)])))
    | (colunaInicio == colunaFim) = ((casaInicio > endCell)          
                    && (foldr (&&) True  (map (estaVazio estado) [(casaInicio - 1),(casaInicio - 2)..(casaFim + 1)])))
                  || ((not (casaInicio > endCell))            
                    && (foldr (&&) True  (map (estaVazio estado) [(casaInicio + 1),(casaInicio + 2)..(casaFim - 1)])))
    | otherwise = False
    where
        linhaInicio = casaInicio `div` 8
        colunaInicio = casaInicio `mod` 8
        linhaFim   = casaFim   `div` 8
        colunaFim   = casaFim   `mod` 8

estaVazio :: EstadoJogo -> Int -> Bool
estaVazio estado index = ((getSquareAt estado index) == Empty)

verificaMovimentoCavalo :: Int -> Int -> Bool
verificaMovimentoCavalo inicio fim =
    (\linhasMovidas colunasMovidas ->
        (linhasMovidas /= 0) && (colunasMovidas /= 0) && ((linhasMovidas + colunasMovidas) == 3)
    ) (abs $ (inicio `div` 8) - (fim `div` 8)) (abs $ (inicio `mod` 8) - (fim `mod` 8))
