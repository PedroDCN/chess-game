module Utilitarios where

import Tipos

-- Métodos para auxilixar o desenvolvimento da GUI

inverteCor :: CorPeca -> CorPeca
inverteCor cor
    | cor == Branco = Preto
    | cor == Preto  = Branco
    | otherwise     = SemCor