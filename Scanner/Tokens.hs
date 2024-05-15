module Tokens where

data Token
  = YYSymbol
  | YYToken
  | YYCHAR Char
  | YYEOF
  | CASE
  | CLASS
  | DEF
  | ELSE
  | EXTENDS
  | IF
  | MATCH
  | NATIVE
  | NEW
  | NULL
  | OVERRIDE
  | SUPER
  | THIS
  | VAR
  | WHILE
  | STR_LIT String
  | INT_LIT Int
  | BOOL_LIT Bool
  | TYPEID String
  | OBJECTID String
  | EQEQ
  | LE
  | ARROW
  | ERROR String
  | UNARY
  deriving (Eq, Show)
