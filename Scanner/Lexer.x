{
module Lexer where

import Tokens
import Control.Monad.State
import Data.IORef
import System.IO
import qualified Data.Text as T
}

%wrapper "monadUserState"

$whitespace = [ \t\r]
$newline = [\n]
$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [$alpha$digit]

state :-
<0>    $white               { \_ -> () }
<0>    $newline             { \_ -> () }          
<0>    "--".*[\n]           { \_ -> () }          
<0>    "//".*               { \_ -> () }
<0>    "--".*[\r\n\v]       { \_ -> () }
<0>    [\n\t\r\v]+          { \_ -> () }
-- <0>    /* "**".*[\r\n\v]?   { \_ -> () }
-- <0>    "\\(\\*"(.*|\r|\n\v)*"\\*\\)"   { \_ -> () }
<0>    "<="                 { \_ -> Token LE }
<0>    "<"                  { \_ -> Token (YYCHAR '<') }
<0>    "=="                 { \_ -> Token EQEQ }
<0>    "=>"                 { \_ -> Token ARROW }
<0>    "("                  { \_ -> Token (YYCHAR '(') }
<0>    ")"                  { \_ -> Token (YYCHAR ')') }
<0>    "{"                  { \_ -> Token (YYCHAR '{') }
<0>    "}"                  { \_ -> Token (YYCHAR '}') }
<0>    "["                  { \_ -> Token (YYCHAR '[') }
<0>    "]"                  { \_ -> Token (YYCHAR ']') }
<0>    ":"                  { \_ -> Token (YYCHAR ':') }
<0>    ";"                  { \_ -> Token (YYCHAR ';') }
<0>    "="                  { \_ -> Token (YYCHAR '=') }
<0>    "."                  { \_ -> Token (YYCHAR '.') }
<0>    ","                  { \_ -> Token (YYCHAR ',') }
<0>    "+"                  { \_ -> Token (YYCHAR '+') }
<0>    "/"                  { \_ -> Token (YYCHAR '/') }
<0>    "*"                  { \_ -> Token (YYCHAR '*') }
<0>    "-"                  { \_ -> Token (YYCHAR '-') }
<0>    "@"                  { \_ -> Token UNARY }
<0>    "not"                { \_ -> Token UNARY }
<0>    "isvoid"             { \_ -> Token UNARY }
<0>    "<-"                 { \_ -> Token UNARY }
<0>    "if"                 { \_ -> Token IF }
<0>    "else"               { \_ -> Token ELSE }
<0>    "class"              { \_ -> Token CLASS }
<0>    "def"                { \_ -> Token DEF }
<0>    "case"               { \_ -> Token CASE }
<0>    "override"           { \_ -> Token OVERRIDE }
<0>    "var"                { \_ -> Token VAR }
<0>    "while"              { \_ -> Token WHILE }
<0>    "this"               { \_ -> Token THIS }
<0>    "super"              { \_ -> Token SUPER }
<0>    "native"             { \_ -> Token NATIVE }
<0>    "extends"            { \_ -> Token EXTENDS }
<0>    "match"              { \_ -> Token MATCH }
<0>    "null"               { \_ -> Token NULL }
<0>    "new"                { \_ -> Token NEW }
<0>    "true"               { \_ -> Token (BOOL_LIT True) }
<0>    "false"              { \_ -> Token (BOOL_LIT False) }
<0>    ">"                  { \_ -> illegalOperatorError ">" }
<0>    ">="                 { \_ -> illegalOperatorError ">=" }
<0>    "|"                  { \_ -> illegalOperatorError "|" }
<0>    "||"                 { \_ -> illegalOperatorError "||" }
<0>    "**"                 { \_ -> illegalOperatorError "**" }
<0>    "%"                  { \_ -> illegalOperatorError "%" }
<0>    ">>"                 { \_ -> illegalOperatorError ">>" }
<0>    "<<"                 { \_ -> illegalOperatorError "<<" }
<0>    "^"                  { \_ -> illegalOperatorError "^" }
<0>    "!"                  { \_ -> illegalOperatorError "!" }
<0>    "object"             { \_ -> illegalKeywordError "object" }
<0>    "abstract"           { \_ -> illegalKeywordError "abstract" }
<0>    "yield"              { \_ -> illegalKeywordError "yield" }
<0>    "val"                { \_ -> illegalKeywordError "val" }
<0>    "private"            { \_ -> illegalKeywordError "private" }
<0>    "trait"              { \_ -> illegalKeywordError "trait" }
<0>    "with"               { \_ -> illegalKeywordError "with" }
<0>    "None"               { \_ -> illegalKeywordError "None" }
<0>    "Exception"          { \_ -> illegalKeywordError "Exception" }
<0>    "to"                 { \_ -> illegalKeywordError "to" }
<0>    "import"             { \_ -> illegalKeywordError "import" }
<0>    "type"               { \_ -> illegalKeywordError "type" }
<0>    "package"            { \_ -> illegalKeywordError "package" }
<0>    [a-z][$alphanum]*    { \s -> Token (OBJECTID s) }
<0>    [A-Z][$alphanum]*    { \s -> Token (TYPEID s) }
<0>    0|[1-9][$digit]*     { \s -> Token (INT_LIT (read s)) }
<0>    ">"                  { \s -> Token (ERROR ("Illegal Operator:" ++ s)) }
<0>    "\""                 { \_ -> ((), state_string) }
<0>    "\"\"\""             { \_ -> ((), state_triple_string) }
<0>    "<<EOF>>"              { \_ -> Token YYEOF }
<state_string>    "\""                { \_ -> exitString }               
<state_string>    "\\n"               { \_ -> appendBuffer "\n" }         
<state_string>   "\\t"               { \_ -> appendBuffer "\t" }         
<state_string>    "\\b"               { \_ -> appendBuffer "\b" }         
<state_string>    "\\f"               { \_ -> appendBuffer "\f" }         
<state_string>    "\\r"               { \_ -> appendBuffer "\r" }         
<state_string>    "\\\\"              { \_ -> appendBuffer "\\" }        
--<state_string>    "\\\""              { \_ -> appendBuffer "\"" }         
<state_string>    "\\"[.]            { \s -> illegalEscape s }
<state_string>    "\\"\n              { \s -> newlineError s}
<state_string>    \n                  { \s -> unterminatedStringError s}   
<state_string>    .                   { \s -> appendBuffer s }           
<state_triple_string> "\"\"\""           { \_ -> exitTripleString } 
<state_triple_string> "<<EOF>>"            { \_ -> (Token (ERROR "Unterminated triple string at EOF"), Normal) }

{
    data LexerState = LexerState {
        stringError :: Maybe String,
        stringBuffer :: String,
        inBasicFile :: Bool,
        lookaheadToken :: Maybe Token,
        lineCount :: Int
    }

    initialState :: LexerState
    initialState = LexerState {
        stringError = Nothing,
        stringBuffer = "",
        inBasicFile = False,
        lookaheadToken = Nothing,
        lineCount = 1
    }

    type Lexer = StateT LexerState IO

    lexToken :: Lexer Token
    lexToken = do
        st <- get
        input <- liftIO getLine
        case input of
            "" -> return YYEOF 
            _ -> return $ lexTokenFromInput input  

    lexTokenFromInput :: String -> Token
    lexTokenFromInput input = 
        case input of
            "if" -> IF
            "else" -> ELSE
            "class" -> CLASS
            "def" -> DEF
            "case" -> CASE
            "override" -> OVERRIDE
            "var" -> VAR
            "while" -> WHILE
            "this" -> THIS
            "super" -> SUPER
            "native" -> NATIVE
            "extends" -> EXTENDS
            "match" -> MATCH
            "null" -> NULL
            "new" -> NEW
            "true" -> BOOL_LIT True
            "false" -> BOOL_LIT False
            "==" -> EQEQ
            "<=" -> LE
            "=>" -> ARROW
            "not" -> UNARY
            "isvoid" -> UNARY
            "<-" -> UNARY
            _ -> ERROR "Unknown token"

    nextToken :: Lexer Token
    nextToken = do
        st <- get
        case lookaheadToken st of
            Just token -> do
                put st { lookaheadToken = Nothing }
                return token
            Nothing -> do
                token <- lexToken
                put st { lookaheadToken = Just token }
                return token

    hasNext :: Lexer Bool
    hasNext = do
        st <- get
        case lookaheadToken st of
            Just YYEOF -> return False
            Just _ -> return True
            Nothing -> do
                token <- lexToken
                put st { lookaheadToken = Just token }
                return (token /= YYEOF)

    setInBasicFile :: Bool -> Lexer ()
    setInBasicFile flag = modify (\st -> st { inBasicFile = flag })

    appendBuffer :: String -> Lexer ()
    appendBuffer str = modify $ \st -> st { stringBuffer = stringBuffer st `T.append` T.pack str }

    exitString :: Lexer (Token, AlexUserState)
    exitString = do
        st <- get
        let result = stringBuffer st
        put st { stringBuffer = T.empty }
        return (STR_LIT $ T.unpack result, Normal)

    illegalEscape :: String -> Lexer (Token, AlexUserState)
    illegalEscape s = do
        return (ERROR ("Illegal escape sequence: " ++ s), Normal)

    unterminatedStringError :: Lexer (Token, AlexUserState)
    unterminatedStringError s = do
        return (ERROR ("Unterminated string at newline" ++ show s), Normal)

    backslashError :: String -> Lexer (Token, AlexUserState)
    backslashError s = do
        return (ERROR ("Backlash followed by: " ++ show s), Normal)

    newlineError :: Lexer (Token, AlexUserState)
    unterminatedStringError s = do
        return (ERROR ("Newlines cannot be followed by: " ++ show s), Normal)

    eofInStringError :: Lexer (Token, AlexUserState)
    eofInStringError = do
        return (ERROR "EOF encountered within string literal", Normal)

    runLexer :: [Token] -> Lexer [Token]
    runLexer acc = do
        hasMore <- hasNext
        if hasMore
            then do
                token <- nextToken
                runLexer (token : acc)
            else
                return (reverse acc)
}