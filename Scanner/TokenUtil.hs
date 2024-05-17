module Scanner.TokenUtil where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Scanner.Tokens (Position, Token(..))

removeFirstLast :: [a] -> [a]
removeFirstLast xs@(_:_) = tail (init xs)
removeFirstLast _ = []

convertSpecialEscapeCharacters :: String -> String
convertSpecialEscapeCharacters ('\\':anyChar:list) = convertEscape anyChar : convertSpecialEscapeCharacters list
  where
    convertEscape newChar =
      case newChar of
        'b' -> '\b'
        't' -> '\t'
        'n' -> '\n'
        'f' -> '\f'
        '\n' -> '\n'
        _ -> newChar
convertSpecialEscapeCharacters (headList:tailList) = headList : convertSpecialEscapeCharacters tailList
convertSpecialEscapeCharacters [] = []

toStringToken :: String -> Position -> Token
toStringToken string position =
  StringLiteral (convertSpecialEscapeCharacters (removeFirstLast string)) position

stringKeywordMap :: Map String (Position -> Token)
stringKeywordMap =
  M.fromList
    [ ("class", CLASS)
    , ("super", SUPER)
    , ("override", OVERRIDE)
    , ("extends", EXTENDS)
    , ("native", NATIVE)
    , ("match", MATCH)
    , ("null", NULL)
    , ("if", IF)
    , ("else", ELSE)
    , ("var", VAR)
    , ("true", TRUE)
    , ("false", FALSE)
    , ("while", WHILE)
    , ("case", CASE)
    , ("new", NEW)
    , ("this", THIS)
    ]

treatKeyword :: (String -> Position -> Token) -> String -> Position -> Token
treatKeyword tokenFactory identifier =
  let keywordLookup = calculateKeyLookup identifier
  in fromMaybe (tokenFactory identifier) (M.lookup keywordLookup stringKeywordMap)

calculateKeyLookup :: String -> String
calculateKeyLookup identifier@(headIdentifier:tailIdentifier) =
  let lowerCaseIdentifier = map toLower identifier
  in if "true" == lowerCaseIdentifier || "false" == lowerCaseIdentifier
       then headIdentifier : map toLower tailIdentifier
       else lowerCaseIdentifier
calculateKeyLookup [] = []

stringOperatorMap :: Map String (Position -> Token)
stringOperatorMap =
  M.fromList
    [ ("[", LB)
    , ("]", RB)
    , ("{", RBRACE)
    , ("}", LBRACE)
    , ("(", LPAREN)
    , (")", RPAREN)
    , ("<", LT)
    , ("<=", LE)
    , ("==", EQEQ)
    , (":", COLON)
    , (";", SEMICOLON)
    , (".", PERIOD)
    , (",", COMMA)
    , ("+", PLUS)
    , ("-", MINUS)
    , ("*", TIMES)
    , ("/", DIV)
    , ("=>", ARROW)
    , ("=", ASSIGN)
    ]

toOperator :: String -> Position -> Token
toOperator operator pos = 
  case M.lookup operator stringOperatorMap of
    Just tokenConstructor -> tokenConstructor pos
    Nothing -> ERROR (head operator) pos
