module Parser.Utils where

import Scanner.Tokens (Token(..), Position(..))

type Symbol = String

toSymbol :: String -> Symbol
toSymbol = id

toExpr :: String -> Token
toExpr str = STRING_LIT { getStrVal = str, getPosition = Position 0 0 }
