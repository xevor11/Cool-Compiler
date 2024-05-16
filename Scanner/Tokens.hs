module Scanner.Tokens
  ( Token(..)
  , Position(..)
  ) where

data Position = Position
  { getLine :: Int
  , getCol :: Int
  } deriving (Eq, Show, Read)

data Token
  = STRING_LIT { getStrVal :: String , getPosition :: Position }
  | INT_LIT { getValue :: Int , getPosition :: Position }
  | BOOL_LIT { getValue :: Bool , getPosition :: Position }
  | TYPEID { getName :: String , getPosition :: Position }
  | OBJECTID { getName :: String , getPosition :: Position }
  | OVERRIDE { getPosition :: Position }
  | SUPER { getPosition :: Position }
  | VAR { getPosition :: Position }
  | MATCH { getPosition :: Position }
  | NATIVE { getPosition :: Position }
  | NULL { getPosition :: Position }
  | CLASS { getPosition :: Position }
  | EXTENDS { getPosition :: Position }
  | IF { getPosition :: Position }
  | THEN { getPosition :: Position }
  | ELSE { getPosition :: Position }
  | TRUE { getPosition :: Position }
  | FALSE { getPosition :: Position }
  | WHILE { getPosition :: Position }
  | CASE { getPosition :: Position }
  | NEW { getPosition :: Position }
  | ARROW { getPosition :: Position }
  | LT { getPosition :: Position }
  | LE { getPosition :: Position }
  | EQEQ { getPosition :: Position }
  | YYEOF
  | LB { getPosition :: Position }
  | RB { getPosition :: Position }
  | RBRACE { getPosition :: Position }
  | LBRACE { getPosition :: Position }
  | LPAREN { getPosition :: Position }
  | RPAREN { getPosition :: Position }
  | COLON { getPosition :: Position }
  | SEMICOLON { getPosition :: Position }
  | PERIOD { getPosition :: Position }
  | COMMA { getPosition :: Position }
  | PLUS { getPosition :: Position }
  | MINUS { getPosition :: Position }
  | TIMES { getPosition :: Position }
  | DIV { getPosition :: Position }
  | ARROW { getPosition :: Position }
  | ASSIGN { getPosition :: Position }
  | INVALIDERROR { getPosition :: Position }
  | EOFERROR { getPosition :: Position }
  | ERROR { getCharacterError :: Char, getPosition :: Position }
  deriving (Eq, Show, Read)