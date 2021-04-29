module Estilo where

import Data.List

colunas = words "a b c d e f g h"
tamanhoBorda = (720, 720)
black = "black"
white = "white"

cssBoard :: [(String, String)]
cssBoard = [("width", show (fst tamanhoBorda) ++ "px"), ("height", show (snd tamanhoBorda) ++ "px"), ("border", "2px solid #404040"), ("box-sizing", "content-box"), ("border-radius", "4px"),
            ("padding", "0px")]

cssContainerBoard :: [(String, String)]
cssContainerBoard = [("top", "50%"), ("left", "30%"), ("width", "50%"), 
        ("position", "absolute"), ("transform", "translateY(-50%)"), 
        ("-webkit-transform", "translateY(-50%)"), ("-moz-transform", "translateY(-50%)")]

cssContainerPrincipal :: [(String, String )]
cssContainerPrincipal = [("position", "absolute"), ("width", "100%"), ("height", "100%"), 
        ("background-color","#ccc")]

cssSquare :: [(String, String)]
cssSquare = [("float", "left"), ("position", "relative"), ("width", show 90 ++ "px") , ("height", show 90 ++ "px"),
            ("-webkit-user-select", "none"), ("-khtml-user-select", "none"),
            ("-moz-user-select", "none"), ("-ms-user-select", "none"), ("user-select", "none")]

cssWhite :: [(String, String)]
cssWhite = [("background-color", "#f0ecd4"), ("color", "#b58863")]

cssBlack :: [(String, String)]
cssBlack = [("background-color", "#789454"), ("color", "#f0d9b5")]

cssHighlight :: [(String, String)]
cssHighlight = [("box-shadow", "inset 0 0 3px 3px yellow")]

-- TODO: calcular tamanho automático de quadrado para ajustar a tamanho qualquer do tabuleiro
calculaTamanhoQuadrado' = let width = fst tamanhoBorda - 1 in
                         diminuiERetorna width
                        where
                            diminuiERetorna tam = if tam `mod` 8 /= 0 && tam > 0
                                                then diminuiERetorna tam-1
                                                else tam `mod` 8

produzLista' :: [[String]]
produzLista' = reverse $ foldl (\acc x -> f x:acc) [] colunas
    where
        f a = foldl (\acc x -> (a ++ show x):acc) [] [1..8]

-- produz lista com todas as casas do tabuleiro - a1 a2 a3 ... h1 h2 h3
listaCasas :: [String]
listaCasas = concat $ transpose produzLista'