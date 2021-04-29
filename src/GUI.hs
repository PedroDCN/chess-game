module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.List
import Estilo

config :: Window -> UI()
config window = do
    return window # set UI.title "Tabuleiro"

    -- definição tabuleio e notações
    tab <- UI.div #. "tabuleiro"
        # set style cssBoard

    -- div com tabuleiro e notações
    divtab <- UI.div #. "tab"
        #+ [element tab]
        # set style [("top", "50%"), ("left", "30%"), ("width", "50%"), 
        ("position", "absolute"), ("transform", "translateY(-50%)"), 
        ("-webkit-transform", "translateY(-50%)"), ("-moz-transform", "translateY(-50%)")]
        -- #+ [element tab, element divntc2, element divntc1]

    -- adiciona casas no tabuleiro
    adicionacasas (element tab) Branca

    -- definição textos invisíveis para guardar jogadas
    j1 <- UI.string "" #. "j1" # set UI.id_ "j1"
    j2 <- UI.string "" #. "j2" # set UI.id_ "j2"
    divinvisivel <- UI.div #. "mov" -- div invisível que guarda as casas da jogada atual
        # set style [("display", "none")]
        #+ [element j1, element j2]

    -- adiciona div do tabuleiro e dos botões em div principal
    principal <- UI.div #. "principal"
        -- # set style [("width", "50%"), ("margin", "auto"), ("border", "2px solid blue"), ("padding", "5px")]
        #+ [element divtab, element divinvisivel]
        # set style [("position", "absolute"), ("width", "100%"), ("height", "100%"), 
        ("background-color","#ccc")]
        -- # set style [("padding", "5px"), ("margin", "0"), ("position", "absolute"), ("top", "50%"), ("left", "50%"), ("transform", "translate(-50%,-50%)"), ("-webkit-transform", "translate(50%, 50%)")]        
        -- #+ [element divtab, element divbtns]

    

    -- adiciona div principal no body
    getBody window #+ [element principal]

    -- adicionando peças na posição inicial
    adicionapecas

    fazHoverPecas -- define funções de efeito de hover in e out de todas as casas do tabuleiro

    fazMovimentoCasas -- define funções para movimento de peças ao clicar para qualquer casa do tabuleiro

-- ================================================================================================================================================    


addJogada f = do
    w <- askWindow
    j1 <- getElementsByClassName w "j1"
    j2 <- getElementsByClassName w "j2"
    let pos1 = head j1
    let pos2 = head j2

    -- log para debug
    text1 <- callFunction $ pegaTexto "j1"
    text2 <- callFunction $ pegaTexto "j2"
    -- consoleLog ("antes da jogada: " ++ text1)
    -- consoleLog ("antes da jogada: " ++ text2)

    if text1 == f then return () else do
        if null text1 then do
            element pos1 # set UI.text f

            element pos1 # set style cssHighlight

            -- log para debug
            -- t1 <- callFunction $ pegaTexto "j1"
            -- consoleLog ("depois da jogada: " ++ t1)

            return ()
        else do
            if null text2 then do
                element pos2 # set UI.text f

                -- log para debug
                -- t2 <- callFunction $ pegaTexto "j2"
                -- consoleLog ("depois da jogada: " ++ t2)

                movePeca text1 f
                element pos1 # set UI.text ""
                element pos2 # set UI.text ""

                callFunction $ removeSombraCasa text1

                return ()
            else do
                return ()


adicionacasas widg cor = do
    -- w <- askWindow
    add 64 widg cor
    return ()

add :: Int -> UI Element -> Cor -> UI ()
add 0 w cor = return ()
add n w cor = do
        -- t <- UI.string ("div " ++ show (64 - n + 1) ++ " " ++ listaCasas !! (64 - n) ++ " ")      -- texto padrão para colocar na casa para reconhecimento e testes
        d <- UI.div #. ((listaCasas !! (64 - n)) ++ " " ++ (if cor == Branca then white else black))
            # set style (cssSquare ++ (if cor == Branca then cssWhite else cssBlack))
            # set UI.id_ (listaCasas !! (64 - n))
            -- #+ [element t]
        w #+ [element d]
        -- runFunction (ffi ("console.log(\'" ++ "div adicionada: " ++ show (64 - n + 1) ++ " " ++ listaCasas !! (64 - n) ++ "  cor do quadrado: " ++ show cor ++ "\')")::JSFunction ())
        add (n-1) w (if (64 - n + 1) `mod` 8 == 0 then cor else troca cor)


-- adiciona peças na posição inical no tabulerio
adicionapecas = do
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

loadPeca :: String -> UI Element
loadPeca peca = do
    url <- loadFile "image/png" ("src/pb/" ++ peca ++".png")
    img <- mkElement "img" # set UI.width 90 # set UI.src url
    mkElement "div" #. ("peca-" ++ peca) #+ [element img]

addPeca :: String -> String -> UI ()
addPeca casa peca = do
    window <- askWindow
    c <- UI.getElementsByClassName window casa
    cp <- loadPeca peca
    element (head c) # set children [] -- limpo o children pra ficar vazio e colocar peça nova
    element (head c) #+ [element cp]
    return ()

movePeca casai casaf = do -- usar função addPeca para a casa final - addPeca casai tipoPeca - reaproveitar
    if casai == "" then return () else do
        temPeca <- verificaTemPecaCasa casai
        if temPeca then do
            --usa função js para dar get do valor da div da casa inicial
            valorCasaini <- callFunction $ pegaTipoPecaCasa casai
            window <- askWindow
            ci <- UI.getElementsByClassName window casai
            cf <- UI.getElementsByClassName window casaf
            let casaIni = head ci
            let casaFin = head cf
            let tipoPeca = drop 5 valorCasaini
            -- consoleLog tipoPeca
            cp <- loadPeca tipoPeca -- cria div com imagem da peça da casa inicial
            element casaIni # set children [] -- limpa casa de inicio
            element casaFin # set children [] -- limpa casa de final para colocar peça
            -- TODO: adicionar peça removida (caso exista) na lista de peças capturadas
            element casaFin #+ [element cp]
            return ()
        else do return ()

consoleLog' :: String -> JSFunction ()
consoleLog' = ffi "console.log(%1)"

consoleLog :: String -> UI ()
consoleLog = runFunction . consoleLog'

consoleL :: String -> UI()
consoleL = runFunction . ffi "console.log(%1)"

pegaTexto :: String -> JSFunction String
pegaTexto = ffi "document.getElementsByClassName(%1).item(0).innerHTML"

pegaCasa :: String -> JSFunction String -- pega o class da casa "c1" -> div c1
pegaCasa = ffi "document.getElementsByClassName(%1).item(0).classList"

pegaEstiloCasa :: String -> JSFunction String
pegaEstiloCasa = ffi "document.getElementsByClassName(%1).item(0).attributes.getNamedItem(\'style\').value"

pegaTipoPecaCasa :: String -> JSFunction String
pegaTipoPecaCasa = ffi "document.getElementsByClassName(%1).item(0).firstElementChild.attributes.getNamedItem(\'class\').value"

pegaClasseCasa :: String -> JSFunction String
pegaClasseCasa = ffi "document.getElementsByClassName(%1).item(0).attributes.getNamedItem(\'class\').value"

depPegaTipo :: String -> UI ()
depPegaTipo casa = do
    txt <- callFunction $ pegaTipoPecaCasa casa
    consoleLog txt

verificaTemPecaCasa :: String -> UI Bool
verificaTemPecaCasa casa = do
    t <- callFunction $ pegaTexto casa
    if t == "" then return False else return True

pegaEstiloSombraCasa :: String -> JSFunction String
pegaEstiloSombraCasa = ffi "window.getComputedStyle(document.getElementsByClassName(%1).item(0)).getPropertyValue(\"box-shadow\")"

removeSombraCasa :: String -> JSFunction ()
removeSombraCasa = ffi "document.getElementsByClassName(%1).item(0).style.removeProperty(\'box-shadow\')"

getUIElementCasa :: String -> UI Element
getUIElementCasa casa = do
    window <- askWindow
    elemento <- UI.getElementsByClassName window casa
    return (head elemento)

limpaConteudo :: String -> JSFunction ()
limpaConteudo = ffi "document.getElementsByClassName(%1).item(0).innerHTML = vara6"

data Cor = Sem | Preta | Branca deriving (Eq, Ord, Show, Read, Bounded, Enum)

troca Branca = Preta
troca Preta = Branca

fazHoverPecas :: UI ()
fazHoverPecas = do
    mapM_ fazHoverPeca listaCasas 

fazHoverPeca :: String -> UI ()
fazHoverPeca x = do
    px <- getUIElementCasa x

    on UI.hover px $ const $ do
        element px # set style cssHighlight
    
    on UI.leave px $ const $ do
        text <- callFunction $ pegaTexto "j1"
        if text == x then return () else do
            callFunction $ removeSombraCasa x

fazMovimentoCasas :: UI ()
fazMovimentoCasas = do
    mapM_ fazMovimentoPeca listaCasas

fazMovimentoPeca :: String -> UI ()
fazMovimentoPeca x = do     -- função genérica adiciona bind de click para movimento de peça na casa 'x'
    px <- getUIElementCasa x

    on UI.click px $ const $ do
        addJogada x