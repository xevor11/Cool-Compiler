module Parser.TerminalNode where

type Identifier = String
type Type = String

toSymbol :: String -> Symbol
toSymbol = id
toExpr :: String -> Token
toExpr str = STRING_LIT { getStrVal = str, getPosition = Position 0 0 }