module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List
import Estilo
import Data.Maybe
import Tipos
import Xadrez
import LogicaValidar
import LogicaJogar
import LogicaXeque

config :: Window -> UI ()
config window = do
    return window # set UI.title "Xadrez 2D" -- define título da página

    -- definição tabuleio
    tab <- UI.div #. "tabuleiro"
        # set style cssBoard

    -- div container do tabuleiro
    divtab <- UI.div #. "tab"
        #+ [element tab]
        # set style cssContainerBoard

    -- adiciona as casas no tabuleiro - malha quadriculada 8x8
    adicionaCasas (element tab) CasaBranca

    -- definição textos invisíveis para guardar jogada
    j1 <- UI.string "" #. "j1" # set UI.id_ "j1"

    -- definição do estado do jogo
    let jogo = jogoInicial
    --consoleLog' $ show (tabuleiro jogo)
    textoEstadoJogo <- UI.string (show jogo) #. "jogo" -- criada string que guarda o jogo
    divTextoEstado <- UI.div #. "estadoJogo" #+ [element textoEstadoJogo]
    textoVez <- UI.string (show $ getVezDeQuem jogo) #. "vez" -- criada string que guarda a vez de quem no jogo
    divTextoVez <- UI.div #. "estadoVez" #+ [element textoVez]
    estado <- UI.div #. "estado" -- criada div que guarda o texto do jogo e da vez
        #+ [element divTextoEstado, element divTextoVez]

    divinvisivel <- UI.div #. "mov" -- div invisível que guarda as casas da jogada atual e o estado do jogo
        # set style [("display", "none")]
        #+ [element j1, element estado]

    s <- UI.string "True" #. "sit" # set UI.id_ "s"
    s2 <- UI.div #. "s" #+ [element s]
    estado2 <- UI.div #. "situacao"
        #+ [element s2]

    -- adiciona div do tabuleiro e das jogadas na div principal
    principal <- UI.div #. "principal"
        #+ [element divtab, element divinvisivel]
        # set style cssContainerPrincipal


    imgbola1 <- UI.div #. "bolabranca"
        # set style (("display","block"):("background-color", "#e9e9e9") : cssBola)
    imgbola2 <- UI.div #. "bolapreta"
        # set style (("display","none"):("background-color", "#111") : cssBola)
    invs <- UI.div #. "invisivel" # set style [("height", "160px"), ("width", "80px"), ("display", "block")]
    divVez <- UI.div #. "bolinhas" # set style [("top", "50%"), ("left", "80%"), ("width", "5%"), 
        ("position", "absolute"), ("transform", "translateY(-50%)"), 
        ("-webkit-transform", "translateY(-50%)"), ("-moz-transform", "translateY(-50%)")]
        #+ [element imgbola2, element invs, element imgbola1]
    
    -- adiciona div principal no tela
    getBody window #+ [element principal, element estado2, element divVez]

    -- adicionando peças na posição inicial do tabuleiro
    adicionaPecas

    -- define funções de efeito de hover in e out de todas as casas do tabuleiro
    fazHoverPecas

    -- define funções para movimento de peças ao clicar para qualquer casa do tabuleiro
    fazMovimentoCasas

    w <- getBody window
    on UI.mousemove w (\ (x, y) -> do
        cor <- getVez
        if cor == Branca then do
            callFunction $ deixaBolaVisivel "bolabranca"
            getUIElementCasa "bolapreta" # set style [("display", "none")]
        else do
            callFunction $ deixaBolaVisivel "bolapreta"
            getUIElementCasa "bolabranca" # set style [("display", "none")]

        jo <- getEstadoJogo
        let posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos (mudaVez jo)) == Rei) &&
                        (getCorPosicao pos (mudaVez jo) == cor) then
                            pos:acc else acc) [] listaTodasPosicoes
        let posRei = head posRei'
        rei <- getUIElementCasa (transformaPosicao posRei)
        if verificaEstaEmChequeMate (mudaVez jo) then do
            callFunction $ removeSombraCasa $ transformaPosicao posRei
            element rei # set style [("box-shadow", "inset 0 0 3px 3px Firebrick1")]
        else getUIElementCasa "tabuleiro"
        )

-- função para fazer jogada dada o clique de uma peça
addJogada :: String -> UI ()
addJogada f = do -- f é a casa, a2 f5 g4 e5
    w <- askWindow
    cor <- getVez
    j1 <- getElementsByClassName w "j1" -- j1 é o elemento q guarda a primeira jogada
    let pos1 = head j1

    jo <- getEstadoJogo

    -- verifica se está em xeque
    if verificaEstaEmCheque jo
        then do -- verifica se está em xeque mate
        if verificaEstaEmChequeMate jo
            then do
                s <- getElementsByClassName w "sit"
                let sit = head s
                element sit # set children []
                nt <- UI.string "False" #. "sit"
                element sit #+ [element nt]
                let cor = vezDeQuem jo
                let posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos jo) == Rei) &&
                        (getCorPosicao pos jo == cor) then
                            pos:acc else acc) [] listaTodasPosicoes
                let posRei = head posRei'
                rei <- getUIElementCasa (transformaPosicao posRei)
                callFunction $ removeSombraCasa $ transformaPosicao posRei
                element rei # set style [("box-shadow", "inset 0 0 3px 3px Firebrick1")]
                consoleLog' $ "Cheque mate, vitória da " ++ show(alternaCor $ vezDeQuem jo)
        else do
            consoleLog' "em cheque, precisa sair dele"
            casaI <- callFunction $ pegaTexto "j1"
            if casaI == f then do
                element pos1 # set UI.text ""
                callFunction $ removeSombraCasa casaI
                return ()
            else do
                if null casaI then do
                    valorDentroDaCasa <- callFunction $ pegaTexto f
                    if valorDentroDaCasa == "" then return () else do
                        -- verificar se a cor da peça na casa é igual a cor da vez
                        c <- callFunction $ pegaCorPecaCasa f
                        if getCorDeTipoPeca' c == cor then do
                            element pos1 # set UI.text f
                            element pos1 # set style cssHighlight
                            return ()
                        else do
                            consoleLog' $ "não é a vez da cor : " ++ show (alternaCor cor)
                else do -- movimenta no jogo
                    let depoisJo' = fazerJogada (Jogada (transformaNotacao casaI) (transformaNotacao f)) jo -- transformar casaI e f em pares de ints seguindo padrão interno
                    let depoisJo = if verificaEstaEmCheque (mudaVez depoisJo') then jo else depoisJo'
                    if tabuleiro jo == tabuleiro depoisJo then do
                        consoleLog' "movimento inválido, tente jogar novamente"
                        element pos1 # set UI.text ""
                        callFunction $ removeSombraCasa casaI
                        return ()
                    else do
                        setEstadoJogo depoisJo
                        setVez $ getVezDeQuem depoisJo
                        -- movimenta ui
                        movePeca casaI f
                        element pos1 # set UI.text ""
                        callFunction $ removeSombraCasa casaI
                        consoleLog' $ "movendo peça de " ++ casaI ++ " para " ++ f
                        return ()

                        if verificaEstaEmChequeMate depoisJo -- verifica se deu mate no inimigo
                            then do
                                s <- getElementsByClassName w "sit"
                                let sit = head s
                                element sit # set children []
                                nt <- UI.string "False" #. "sit"
                                element sit #+ [element nt]
                                let cor = vezDeQuem jo
                                let posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos jo) == Rei) &&
                                        (getCorPosicao pos jo == cor) then
                                            pos:acc else acc) [] listaTodasPosicoes
                                let posRei = head posRei'
                                rei <- getUIElementCasa (transformaPosicao posRei)
                                callFunction $ removeSombraCasa $ transformaPosicao posRei
                                element rei # set style [("box-shadow", "inset 0 0 3px 3px Firebrick1")]
                                consoleLog' $ "cheque mate acabou, vitoria da " ++ show cor
                            else return ()

    -- não está em cheque
    else
        if verificaPosicaoAfogado jo
            then do
                s <- getElementsByClassName w "sit" -- j1 é o elemento q guarda a primeira jogada
                let sit = head s
                element sit # set children []
                nt <- UI.string "False" #. "sit"
                element sit #+ [element nt]
                consoleLog' "Rei afogado, empate"
        else do
            -- faz jogada normal
            consoleLog' "situação de jogada normal"
            casaI <- callFunction $ pegaTexto "j1"
            if casaI == f then do
                element pos1 # set UI.text ""
                callFunction $ removeSombraCasa casaI
                return ()
            else do
                if null casaI then do
                    valorDentroDaCasa <- callFunction $ pegaTexto f
                    if valorDentroDaCasa == "" then return () else do
                        -- verificar se a cor da peça na casa é igual a cor da vez
                        c <- callFunction $ pegaCorPecaCasa f
                        if getCorDeTipoPeca' c == cor then do
                            element pos1 # set UI.text f
                            element pos1 # set style cssHighlight
                            return ()
                        else do
                            consoleLog' $ "não é a vez da cor : " ++ show (alternaCor cor)
                else do -- movimenta no jogo
                    let depoisJo' = fazerJogada (Jogada (transformaNotacao casaI) (transformaNotacao f)) jo -- transformar casaI e f em pares de ints seguindo padrão interno
                    let depoisJo = if verificaEstaEmCheque (mudaVez depoisJo') then jo else depoisJo'
                    if tabuleiro jo == tabuleiro depoisJo then do
                        consoleLog' "movimento inválido, tente jogar novamente"
                        element pos1 # set UI.text ""
                        callFunction $ removeSombraCasa casaI
                        return ()
                    else do
                        setEstadoJogo depoisJo
                        setVez $ getVezDeQuem depoisJo
                        -- movimenta ui
                        movePeca casaI f
                        element pos1 # set UI.text ""
                        callFunction $ removeSombraCasa casaI
                        consoleLog' $ "movendo peça de " ++ casaI ++ " para " ++ f
                        return ()

                        if verificaEstaEmChequeMate depoisJo -- verifica se deu mate no inimigo
                            then do
                                s <- getElementsByClassName w "sit"
                                let sit = head s
                                element sit # set children []
                                nt <- UI.string "False" #. "sit"
                                element sit #+ [element nt]
                                let cor = vezDeQuem jo
                                let posRei' = foldl (\acc pos -> if (tipo (getPecaPosicao pos jo) == Rei) &&
                                        (getCorPosicao pos jo == cor) then
                                            pos:acc else acc) [] listaTodasPosicoes
                                let posRei = head posRei'
                                rei <- getUIElementCasa (transformaPosicao posRei)
                                callFunction $ removeSombraCasa $ transformaPosicao posRei
                                element rei # set style [("box-shadow", "inset 0 0 3px 3px Firebrick1")]
                                consoleLog' $ "cheque mate acabou, vitoria da " ++ show cor
                            else return ()

-- Função que adiciona as casas do tabuleiro, seguindo o padrão quadriculado do jogo
adicionaCasas :: UI Element -> CorCasa -> UI ()
adicionaCasas widg cor = do
    add' 64 widg cor
    return ()

-- função auxiliar de adicionaCasas
add' :: Int -> UI Element -> CorCasa -> UI ()
add' 0 w cor = return ()
add' n w cor = do
        d <- UI.div #. ((listaCasas !! (64 - n)) ++ " " ++ (if cor == CasaBranca then white else black))
            # set style (cssSquare ++ (if cor == CasaBranca then cssWhite else cssBlack))
            # set UI.id_ (listaCasas !! (64 - n))
        w #+ [element d]
        -- consoleLog' ("div " ++ (listaCasas !! (64-n)))
        add' (n-1) w (if (64 - n + 1) `mod` 8 == 0 then cor else troca cor)


-- função que adiciona as peças na posição inicial do jogo
adicionaPecas = do
    -- w <- askWindow
    -- adiciona peças brancas
    addPeca "a1" "wr"
    addPeca "b1" "wn"
    addPeca "c1" "wb"
    addPeca "d1" "wq"
    addPeca "e1" "wk"
    addPeca "f1" "wb"
    addPeca "g1" "wn"
    addPeca "h1" "wr"
    -- adiciona peões brancos
    addPeca "a2" "wp"
    addPeca "b2" "wp"
    addPeca "c2" "wp"
    addPeca "d2" "wp"
    addPeca "e2" "wp"
    addPeca "f2" "wp"
    addPeca "g2" "wp"
    addPeca "h2" "wp"
    -- adiciona pecas pretas
    addPeca "a8" "br"
    addPeca "b8" "bn"
    addPeca "c8" "bb"
    addPeca "d8" "bq"
    addPeca "e8" "bk"
    addPeca "f8" "bb"
    addPeca "g8" "bn"
    addPeca "h8" "br"
    -- adiciona peões pretos
    addPeca "a7" "bp"
    addPeca "b7" "bp"
    addPeca "c7" "bp"
    addPeca "d7" "bp"
    addPeca "e7" "bp"
    addPeca "f7" "bp"
    addPeca "g7" "bp"
    addPeca "h7" "bp"

-- função auxiliar para carregar na janela a imagem de uma peça
loadPeca' :: String -> UI Element
loadPeca' peca = do
    url <- loadFile "image/png" ("src/pb/" ++ peca ++".png")
    img <- mkElement "img" # set UI.width 90 # set UI.src url
    mkElement "div" #. (peca++"peca") #+ [element img]

-- função que adiciona uma peça na casa especificada
addPeca :: String -> String -> UI ()
addPeca casa peca = do
    window <- askWindow
    c <- UI.getElementsByClassName window casa
    cp <- loadPeca' peca
    element (head c) # set children [] -- limpo o children pra ficar vazio e colocar peça nova
    element (head c) #+ [element cp]
    return ()

-- função que move uma peça da casaInicial (ex: a2) até a casaFinal especificada.
-- Caso a casa inicial seja vazia, não realiza nenhum movimento.
-- TODO: adicionar peça removida (caso exista) na lista de peças capturadas
movePeca casai casaf = do
    if casai == "" then return () else do
        temPeca <- verificaTemPecaCasa casai
        if temPeca then do
            --usa função js para dar get do valor da div da casa inicial
            valorCasaini <- callFunction $ pegaTipoPecaCasa casai
            window <- askWindow
            ci <- UI.getElementsByClassName window casai
            let casaIni = head ci
            let tipoPeca = take 2 valorCasaini
            element casaIni # set children [] -- limpa casa de início
            addPeca casaf tipoPeca -- usa função addPeca para adicionar peça na casaFinal do movimento
            return ()
        else do return ()

-- função que verifica se dada uma casa (ex: a2), existe uma peça nela, retorna um UI Bool.
verificaTemPecaCasa :: String -> UI Bool
verificaTemPecaCasa casa = do
    t <- callFunction $ pegaTexto casa
    if t == "" then return False else return True

-- função que retorna o UI Element para um dado valor de class de uma casa
getUIElementCasa :: String -> UI Element
getUIElementCasa casa = do
    window <- askWindow
    elemento <- UI.getElementsByClassName window casa
    return (head elemento)

-- função que cria os eventos de hover das casas,
-- dando highlight nas casas ao passar o mouse por cima delas
fazHoverPecas :: UI ()
fazHoverPecas = do
    mapM_ fazHoverPeca' listaCasas 

-- função auxiliar que cria o evento de hover para uma casa dada
fazHoverPeca' :: String -> UI ()
fazHoverPeca' x = do
    px <- getUIElementCasa x

    on UI.hover px $ const $ do
        situacao <- callFunction $ pegaTexto "sit"
        if situacao == "True" then
            element px # set style cssHighlight
        else
            getUIElementCasa "s"

    on UI.leave px $ const $ do
        text <- callFunction $ pegaTexto "j1"
        situacao <- callFunction $ pegaTexto "sit"
        if situacao == "True" then
            if text == x then return () else do
                callFunction $ removeSombraCasa x
        else return ()

-- função que cria os eventos de mover uma peça no tabuleiro por meio de cliques nas casas
fazMovimentoCasas :: UI ()
fazMovimentoCasas = do
    mapM_ fazMovimentoPeca' listaCasas

-- função auxiliar que cria o evento de movimento para uma casa dada
fazMovimentoPeca' :: String -> UI ()
fazMovimentoPeca' x = do  -- função genérica adiciona bind de click para movimento de peça na casa 'x'
    px <- getUIElementCasa x

    on UI.click px $ const $ do
        situacao <- callFunction $ pegaTexto "sit"
        if situacao == "True" then
            addJogada x
        else return ()

-- Tipo de cor das casas do tabuleiro, usado para auxiliar na criação do tabuleiro
data CorCasa = Sem | CasaPreta | CasaBranca deriving (Eq)

-- função para trocar o valor de um elemento do tipo Cor
troca :: CorCasa -> CorCasa
troca CasaBranca = CasaPreta
troca CasaPreta = CasaBranca

-- Funções utilitárias de outras funções

transformaNotacao :: String -> Posicao
transformaNotacao casa = (linha (read [casa !! 1]::Int), coluna [head casa])
    where
        coluna y = fromJust $ lookup y (zip (words "a b c d e f g h") [0..7]) -- coluna a = 0, coluna b = 1 .. coluna h = 7
        linha x = x-1 -- dada linha x no tabuleiro, representação interna é x-1 (ex: 4 = 3, pois vai de 0 a 7 internamente)

transformaPosicao :: Posicao -> String
transformaPosicao pos@(x, y) = coluna y ++ show (linha x)
    where
        coluna y1 = fromJust $ lookup y1 (zip [0..7] (words "a b c d e f g h"))
        linha x1 = x1+1 

getCorDeTipoPeca' :: String -> Cor
getCorDeTipoPeca' (c:cs)
    | c == 'w' = Branca
    | otherwise = Preta

pegaCorPecaCasa :: String -> JSFunction String
pegaCorPecaCasa = ffi "document.getElementsByClassName(%1).item(0).firstElementChild.className"

-- função utilitária que pega o valor de sombra de uma dada casa
pegaEstiloSombraCasa :: String -> JSFunction String
pegaEstiloSombraCasa = ffi "window.getComputedStyle(document.getElementsByClassName(%1).item(0)).getPropertyValue(\"box-shadow\")"

-- função utilitária que remove o highlight de uma dada casa
removeSombraCasa :: String -> JSFunction ()
removeSombraCasa = ffi "document.getElementsByClassName(%1).item(0).style.removeProperty(\'box-shadow\')"

-- função utilitária que pega o tipo da peça para uma casa (ex: a2) dada
pegaTipoPecaCasa :: String -> JSFunction String
pegaTipoPecaCasa = ffi "document.getElementsByClassName(%1).item(0).firstElementChild.attributes.getNamedItem(\'class\').value"

-- função utilitária para pegar texto interno de um elemento dado da tela
pegaTexto :: String -> JSFunction String
pegaTexto = ffi "document.getElementsByClassName(%1).item(0).innerHTML"

-- função utilitária para debug
consoleLog' :: String -> UI()
consoleLog' = runFunction . ffi "console.log(%1)"

getEstadoJogo' :: JSFunction String -- retorna a representação textual do estado do jogo salva
getEstadoJogo' = ffi "document.getElementsByClassName(%1).item(0).firstElementChild.firstElementChild.innerHTML" "estado"

-- função que retorna o elemento do tipo EstadoJogo do player para ser alterado
getEstadoJogo :: UI Jogo
getEstadoJogo = do
    t <- callFunction getEstadoJogo'
    let f = read t::Jogo
    return f

setEstadoJogo :: Jogo -> UI ()
setEstadoJogo ej = do
    w <- askWindow
    p <- UI.getElementsByClassName w "estadoJogo"
    textoEstadoJogo <- UI.string (show ej) #. "jogo"
    let divJogo = head p
    element divJogo # set children [] -- limpa internamente a div = tira o do estado guardado
    element divJogo #+ [element textoEstadoJogo] -- redundante para evitar erros de buffer ao limpar e inserir logo em seguida
    return ()

getVez' :: JSFunction String -- retorna a representação textual do estado do jogo salva
getVez' = ffi "document.getElementsByClassName(%1).item(0).lastElementChild.firstElementChild.innerHTML" "estado"

-- função que retorna o elemento do tipo EstadoJogo do player para ser alterado
getVez :: UI Cor
getVez = do
    t <- callFunction getVez'
    let f = read t::Cor
    return f

setVez :: Cor -> UI ()
setVez vez = do
    w <- askWindow
    p <- UI.getElementsByClassName w "estadoVez"
    textoVezJogo <- UI.string (show vez) #. "vez"
    let divJogo = head p
    element divJogo # set children [] -- limpa internamente a div = tira o do estado guardado
    element divJogo #+ [element textoVezJogo] -- redundante para evitar erros de buffer ao limpar e inserir logo em seguida
    return ()

deixaBolaVisivel :: String -> JSFunction ()
deixaBolaVisivel = ffi "document.getElementsByClassName(%1).item(0).style.removeProperty(\'display\')"