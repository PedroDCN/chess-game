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
        ((linhaInicio == linhaFim) && (abs (colunaInicio - colunaFim)) == 1)            
        || ((colunaInicio == colunaFim) && (abs (linhaInicio - linhaFim)) == 1)         
        || ((abs (linhaInicio - linhaFim))==1 && (abs (colunaInicio - colunaFim)) == 1)
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

verificaMovimentoCavalo :: Int -> Int -> Bool
verificaMovimentoCavalo inicio fim =
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

-- pecaCor deve ser definido em outra classe (Cor da Peça - Preto ou Branco)
-- pegaQuadrado (retorna uma peça que está no quadrado ou Vazio)
-- pegaQuadradoCor (retorna a cor da peça no quadrado passado pro parâmetro)
-- tipo "Vazio" também deve ser definido
-- tipo Branco e Preto deve ser definido na classe Cor
verificaMovimentoPeao :: EstadoJogo -> Int -> Int -> pecaCor -> Bool
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

-- É necessário definir os tipos Humano, Computador, o estado do jogo (para pegar o turno de quem jogou)
-- Também precisamos criar as funções do tabuleiro para pegar informações da peça (Cor e Tipo) que está no quadrado
verificaMovimento :: EstadoJogo -> Int -> Int -> Bool
verificaMovimento estado inicio fim
    | not movimentoValido = False
    | otherwise = case pecaTipo of
        Peao      -> (verificaMovimentoPeao estado inicio fim)
        Cavalo    -> (verificaMovimentoCavalo estado inicio fim)
        Bispo     -> (verificaMovimentoBispo estado inicio fim)
        Rei       -> (verificaMovimentoRei estado inicio fim)
        Dama      -> (verificaMovimentoDama estado inicio fim)
        Torre     -> (verificaMovimentoTorre estado inicio fim)
        otherwise -> False
    where
        quadrado        = (pegaQuadrado estado inicio)
        pecaTipo        = pegaQuadradoTipo quadrado
        pecaCor         = pegaQuadradoCor quadrado
        turno           = pegaTurno estado
        movimentoValido = (inicio >= 0 && inicio <= 63 && fim >= 0 && fim <= 63)
            && (
                ((turno == Humano) && (pecaCor == Branco))
                || ((turno == Computador) && (pecaCor == Preto))
               )
            && (pecaCor /= (pegaQuadradoCor (pegaQuadrado estado fim)))

-- Necessita de implementação em outras classes (Tipo Vazio e método de pegar o quadrado no tabuleiro)
-- Assim como o "EstadoJogo" que armazena a posição das peças no tabuleiro (necessário para saber os quadrados que estão vazios)
estaVazio :: EstadoJogo -> Int -> Bool
estaVazio estado indice = ((pegaQuadrado estado indice) == Vazio)