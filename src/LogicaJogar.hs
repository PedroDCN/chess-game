module LogicaJogar where

import Tipos

{--
-- função que, dada uma posição no tabuleiro e o estado do tabuleiro atual,
-- retorna todos os possíveis movimentos (lista de Movimentos)
-- (a lógica para verificar jogadas possíveis difere
-- de peça pra peça, fazer para cada pessa as jogadas possíveis)
--
-- função que gera todas as jogadas possíveis de se fazer
-- dada um estado do tabuleiro
--
-- cada jogada só é possível de se fazer se seguir as regras de validade
-- da logicaValidar.
--}

{--
-- obs:
--
-- pode fazer para cada peça, verificar com todas as casas do tabuleiro se a jogada
-- é válida, e retornar só as válidas,
-- ou para cada peça, verificar apenas as casas válidas seguindo a regra de movimento dela,
-- e verificar dessas quais são possíveis. Por exemplo um peão só pode
-- se mover pra frente ou suas duas casas diagonais (duas casas para frente se estiver na posição
-- inicial, cobrir também este caso no código), não tem necessidade de verificar se o movimento
-- dele para o final do tabuleiro é válido, e ainda você não pode mover ele caso esteja em cheque por exemplo.
--}

listaMovimentosRei :: Int -> [Int] -> [Int]
listaMovimentosRei _ [] = []
listaMovimentosRei inicio (x:xs) =
    if (inicio`div`8) == (x`div`8) && (abs ((inicio`mod`8) - (x`mod`8))) == 1 then [x] ++ (listaMovimentosRei inicio xs)
    else if (inicio`mod`8) == (x`mod`8) && (abs ((inicio`div`8) - (x`div`8))) == 1 then [x] ++ (listaMovimentosRei inicio xs)
    else if (abs ((inicio`div`8) - (x`div`8))) == 1 && (abs ((inicio`mod`8) - (x`mod`8))) == 1 then [x] ++ (listaMovimentosRei inicio xs)
    else (listaMovimentosRei inicio xs)
    where (x:xs) = [0..63]