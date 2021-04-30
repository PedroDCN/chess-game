module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List
import Estilo
import Data.Maybe
import Utilitarios
import Tipos
import Tabuleiro

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
    adicionaCasas (element tab) Branca

    -- definição textos invisíveis para guardar jogada
    j1 <- UI.string "" #. "j1" # set UI.id_ "j1"
    -- definição de variáveis de estado do jogo
    let estadoJogoInicialPlayer = habilitarMovimento estadoJogoInicialBranco
    let copiaEstadoJogoInicialPlayer = habilitarMovimento estadoJogoInicialBranco
    let estadoJogoInicialBot = desabilitarMovimento estadoJogoInicialPreto
    let vez = Branco

    -- definição div invisível que guarda o esado de jogo atual em forma de string
    textoEstadoPlayer <- UI.string (show estadoJogoInicialPlayer) #. "player"
    textoEstadoPLayer2 <- UI.string (show copiaEstadoJogoInicialPlayer) #. "bu"
    texttoEstadoBot <- UI.string (show estadoJogoInicialBot) #. "bot"
    estadop <- UI.div #. "estadoPlayer" 
        #+ [element textoEstadoPlayer]
    estadobu <- UI.div #. "estadoBackup"
        #+ [element textoEstadoPLayer2]
    estadob <- UI.div #. "estadoBot"
        #+ [element texttoEstadoBot]
    textoVez <- UI.string (show vez) #. "vez"
    vezc <- UI.div #. "vezJ"
        #+ [element textoVez]

    estado <- UI.div #. "estado" -- div que guarda as duas divs de estado
        #+ [element estadop, element estadobu, element vezc, element estadob]

    divinvisivel <- UI.div #. "mov" -- div invisível que guarda as casas da jogada atual e o estado do jogo
        # set style [("display", "none")]
        #+ [element j1, element estado]

    -- adiciona div do tabuleiro e dos botões em div principal
    principal <- UI.div #. "principal"
        #+ [element divtab, element divinvisivel]
        # set style cssContainerPrincipal

    -- adiciona div principal no body
    getBody window #+ [element principal]

    -- adicionando peças na posição inicial
    adicionaPecas

    -- define funções de efeito de hover in e out de todas as casas do tabuleiro
    fazHoverPecas

    -- define funções para movimento de peças ao clicar para qualquer casa do tabuleiro
    fazMovimentoCasas

-- função para fazer jogada dada o clique de uma peça
addJogada :: String -> UI ()
addJogada f = do
    w <- askWindow
    j1 <- getElementsByClassName w "j1"
    let pos1 = head j1
    casaI <- callFunction $ pegaTexto "j1"
    if casaI == f then do
        element pos1 # set UI.text ""
        callFunction $ removeSombraCasa casaI
        return ()
    else do
        if null casaI then do
            element pos1 # set UI.text f
            element pos1 # set style cssHighlight
            return ()
        else do
            movePeca casaI f
            element pos1 # set UI.text ""
            callFunction $ removeSombraCasa casaI
            return ()

-- Função que adiciona as casas do tabuleiro, seguindo o padrão quadriculado do jogo
adicionaCasas :: UI Element -> Cor -> UI ()
adicionaCasas widg cor = do
    add' 64 widg cor
    return ()

-- função auxiliar de adicionaCasas
add' :: Int -> UI Element -> Cor -> UI ()
add' 0 w cor = return ()
add' n w cor = do
        d <- UI.div #. ((listaCasas !! (64 - n)) ++ " " ++ (if cor == Branca then white else black))
            # set style (cssSquare ++ (if cor == Branca then cssWhite else cssBlack))
            # set UI.id_ (listaCasas !! (64 - n))
        w #+ [element d]
        add' (n-1) w (if (64 - n + 1) `mod` 8 == 0 then cor else troca cor)


-- função que adiciona as peças na posição inicial do jogo
-- TODO: fazer mais funcional -> lista de pares casa peça e passar num foldl
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
    mkElement "div" #. ("peca-" ++ peca) #+ [element img]

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
            let tipoPeca = drop 5 valorCasaini
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
        element px # set style cssHighlight
    
    on UI.leave px $ const $ do
        text <- callFunction $ pegaTexto "j1"
        if text == x then return () else do
            callFunction $ removeSombraCasa x

-- função que cria os eventos de mover uma peça no tabuleiro por meio de cliques nas casas
fazMovimentoCasas :: UI ()
fazMovimentoCasas = do
    mapM_ fazMovimentoPeca' listaCasas

-- função auxiliar que cria o evento de movimento para uma casa dada
fazMovimentoPeca' :: String -> UI ()
fazMovimentoPeca' x = do  -- função genérica adiciona bind de click para movimento de peça na casa 'x'
    px <- getUIElementCasa x

    on UI.click px $ const $ do
        addJogada x

-- Tipo de cor das casas do tabuleiro, usado para auxiliar na criação do tabuleiro
data Cor = Sem | Preta | Branca deriving (Eq)

-- função para trocar o valor de um elemento do tipo Cor
troca :: Cor -> Cor
troca Branca = Preta
troca Preta = Branca

-- Funções utilitárias de outras funções

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
-- consoleLog' :: String -> UI()
-- consoleLog' = runFunction . ffi "console.log(%1)"

getEstadoPlayer' :: JSFunction String
getEstadoPlayer' = ffi "document.getElementsByClassName(%1).item(0).firstElementChild.firstElementChild.innerHTML" "estado"

-- função que retorna o elemento do tipo EstadoJogo do player para ser alterado
getEstadoPlayer :: UI EstadoJogo
getEstadoPlayer = do
    t <- callFunction getEstadoPlayer'
    let f = read t::EstadoJogo
    return f

setEstadoPlayer :: EstadoJogo -> UI ()
setEstadoPlayer ej = do
    w <- askWindow 
    p <- UI.getElementsByClassName w "estadoPlayer"
    textoEstadoPlayer <- UI.string (show ej) #. "player"
    let divPlayer = head p
    element divPlayer # set children [] -- limpa internamente a div = tira o do estado guardado
    element divPlayer #+ [element textoEstadoPlayer] -- redundante para evitar erros de buffer ao limpar e inserir logo em seguida
    return ()

getEstadoPlayerBu' :: JSFunction String
getEstadoPlayerBu' = ffi "document.getElementsByClassName(%1).item(0).getElementsByClassName(%2).item(0).firstElementChild.innerHTML" "estado" "estadoBackup"

-- função que retorna o elemento do tipo EstadoJogoBackUp do player para ser alterado
getEstadoPlayerBu :: UI EstadoJogo
getEstadoPlayerBu = do
    t <- callFunction getEstadoPlayerBu'
    let f = read t::EstadoJogo
    return f

setEstadoPlayerBu :: EstadoJogo -> UI ()
setEstadoPlayerBu ej = do
    w <- askWindow 
    p <- UI.getElementsByClassName w "estadoBackup"
    textoEstadoPlayerBu <- UI.string (show ej) #. "Bu"
    let divPlayerBu = head p
    element divPlayerBu # set children [] -- limpa internamente a div = tira o do estado guardado
    element divPlayerBu #+ [element textoEstadoPlayerBu] -- redundante para evitar erros de buffer ao limpar e inserir logo em seguida
    return ()

getEstadoBot' :: JSFunction String
getEstadoBot' = ffi "document.getElementsByClassName(%1).item(0).lastElementChild.firstElementChild.innerHTML" "estado"

-- função que retorna o elemento do tipo EstadoJogo do bot para ser alterado
getEstadoBot :: UI EstadoJogo
getEstadoBot = do
    t <- callFunction getEstadoBot'
    let f = read t::EstadoJogo
    return f

setEstadoBot :: EstadoJogo -> UI ()
setEstadoBot ej = do
    w <- askWindow
    p <- UI.getElementsByClassName w "estadoBot"
    textoEstadoBot <- UI.string (show ej) #. "bot"
    let divBot = head p
    element divBot # set children [] -- limpa internamente a div = tira o do estado guardado
    element divBot #+ [element textoEstadoBot] -- redundante para evitar erros de buffer ao limpar e inserir logo em seguida
    return ()

getVez' :: JSFunction String
getVez' = ffi "document.getElementsByClassName(%1).item(0).getElementsByClassName(%2).item(0).firstElementChild.innerHTML" "estado" "vezJ"

getVez :: UI TipoPeca
getVez = do
    t <- callFunction getVez'
    let f = read t::TipoPeca
    return f

setVez :: TipoPeca -> UI ()
setVez vez = do
    w <- askWindow
    p <- UI.getElementsByClassName w "vezJ"
    textoVez <- UI.string (show vez) #. "vez"
    let divVez = head p
    element divVez # set children []
    element divVez #+ [element textoVez]
    return ()