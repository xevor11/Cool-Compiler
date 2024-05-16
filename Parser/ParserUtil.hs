{-# LANGUAGE FlexibleInstances #-}

module Parser.ParserUtil where

import Data.List (find)

import Lexer.Lexer (runAlex, scanner)
import Lexer.Token
import Parser.AST
import Parser.Parser
       (classesParser, classParser, symbolParser, featuresParser, featureParser, formalsParser, formalParser, expressionParser, expressionsParser, caseParser, casesParser, booleanParser)

scanErrors :: [Token] -> [Token]
scanErrors tokens =
  case find classifyErrorToken tokens of
    Just errorToken -> error $ "Found error token" ++ show errorToken
    Nothing -> tokens
  where
    classifyErrorToken (InvalidCharacterError _ _) = True
    classifyErrorToken (UnterminatedStringError _) = True
    classifyErrorToken (EOFStringError _) = True
    classifyErrorToken (NullCharacterError _) = True
    classifyErrorToken _ = False

stringToAST :: ([Token] -> a) -> String -> a
stringToAST parser code =
  parser $
  case code `runAlex` scanner of
    Right tokens -> tokens

class Parsable a where
  parse :: String -> a

instance Parsable [Class] where
  parse = stringToAST classesParser

instance Parsable Class where
  parse = stringToAST classParser

instance Parsable Symbol where
  parse = stringToAST symbolParser

instance Parsable Feature where
  parse = stringToAST featureParser

instance Parsable [Feature] where
  parse = stringToAST featuresParser

instance Parsable Formal where
  parse = stringToAST formalParser

instance Parsable [Formal] where
  parse = stringToAST formalsParser

instance Parsable Expression where
  parse = stringToAST expressionParser

instance Parsable [Expression] where
  parse = stringToAST expressionsParser

instance Parsable Case where
  parse = stringToAST caseParser

instance Parsable [Cases] where
  parse = stringToAST casesParser

instance Parsable Symbol where
  parse = stringToAST symbolParser

instance Parsable Boolean where
  parse = stringToAST booleanParser