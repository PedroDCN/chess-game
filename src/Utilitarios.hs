module Utilitarios where

import Tipos

-- Métodos para auxilixar o desenvolvimento da GUI

inverteCor :: PecaCor -> PecaCor
inverteCor cor
    | cor == Branco = Preto
    | cor == Preto  = Branco
    | otherwise     = SemCor