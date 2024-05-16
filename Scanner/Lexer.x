{
module Scanner.Lexer  where
import Scanner.Tokens
import Scanner.TokenUtil
}

%wrapper "monad"

$digit = 0-9			
$alpha = [a-zA-Z]		
$alphaNumeric = [a-zA-Z0-9] 


@endingComment = \*\)

@validIdentifier = "_" | $alphaNumeric
@string = [^\"\n] | \\\" | \\\n
@typeIdentifier = [A-Z] @validIdentifier*
@objectIdentifier = [a-z] @validIdentifier*
@reservedOps = "[" | "]" | "{" | "}" | "(" |  ")" | "<" | "=" | ":" | ";" | "." | "," | "+"| "-"| "*" | "/"| "=>"

tokens :-

  <0> $white+				;
  <0> \\n
  <0> @reservedOps          { toParameterizedToken toOperator }
  <0> [0-9]+			    { toParameterizedToken (INT_LIT . read) }
  <0> @typeIdentifier       { toParameterizedToken (treatKeyword TYPEID) }
  <0> @objectIdentifier     { toParameterizedToken (treatKeyword OBJECTID) }

  <0> \" @string* \"        { toParameterizedToken toStringToken } 
  <0> \" @string* \n        { toSingularToken INVALIDERROR }
  <0> \" @string*           { toSingularToken EOFERROR }

  <0> .                     { toParameterizedToken (ERROR . head) }
{

toParameterizedToken :: (String -> Position -> Token) -> AlexInput -> Int -> Alex Token
toParameterizedToken tokenFactory ((AlexPn _ lineNumber colNumber), _, _, str) len = let stringToken = take len str in
                                                       return (tokenFactory stringToken (Position lineNumber colNumber))

toSingularToken :: (Position -> Token) -> AlexInput -> Int -> Alex Token
toSingularToken posTokenCB ((AlexPn _ lineNumber colNumber), _, _, _) _ = return (posTokenCB (Position lineNumber colNumber))

scanner :: Alex [Token]
scanner = do
    token <- alexMonadScan
    case token of
        YYEOF ->  return []
        _ -> do subresult <- scanner; return (token : subresult)

alexEOF = return $ YYEOF
}