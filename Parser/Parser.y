{
module Parser.Parser where
import Data.Char
import Data.Maybe
import Data.List

import qualified Scanner.Tokens as T
import Parser.Utils (toSymbol, toExpr)
import Cool
}

%name classesParser class_list
%name classParser class_decl
%name symbolParser superclass
%name featuresParser feature_list
%name featureParser feature
%name formalsParser var_formals
%name formalsParser var_formals_one_or_more
%name formalsParser formals_one_or_more
%name formalParser formal
%name formalParser var_formal
%name expressionParser expr
%name expressionParser block
%name expressionsParser block
%name expressionsParser actuals
%name expressionsParser expr_list
%name expressionsParser stmt_list
%name casesParser simple_cases
%name caseParser simple_case
%name booleanParser opt_override
%tokentype { T.Token }
%error { parseError }

%token 
  '+'               { T.PLUS }
  '-'               { T.MINUS }
  '*'               { T.TIMES }
  '/'               { T.DIVIDE }
  '<'               { T.LT }
  '<='              { T.LE }
  "=="              { T.EQEQ }
  '='               { T.ASSIGN }
  ';'               { T.SEMICOLON }
  ','               { T.COMMA }
  '.'               { T.PERIOD }
  '!'               { T.NOT }
  '('               { T.LPAREN }
  ')'               { T.RPAREN }
  '{'               { T.LBRACE }
  '}'               { T.RBRACE }
  ':'               { T.COLON }
  '=>'              { T.ARROW }
  "class"           { T.CLASS }
  "extends"         { T.EXTENDS }
  "def"             { T.DEF }
  "else"            { T.ELSE }
  "if"              { T.IF }
  "match"           { T.MATCH }
  "native"          { T.NATIVE }
  "new"             { T.NEW }
  "null"            { T.NULL }
  "override"        { T.OVERRIDE }
  "super"           { T.SUPER }
  "this"            { T.THIS }
  "var"             { T.VAR }
  "while"           { T.WHILE }
  OBJECTID          { T.OBJECTID }
  TYPEID            { T.TYPEID }
  INT_LIT           { T.INT_LIT }
  STR_LIT           { T.STR_LIT }
  BOOL_LIT          { T.BOOL_LIT }

%left '='
%left IF
%left MATCH
%left LE '<'
%left EQEQ
%left '-' '+'
%left '*' '/'
%left UNARY
%left '.'

%%

program : class_list { Program $1 }
        ;

class_list : class_decl { [$1] }
           | class_list class_decl { $2 : $1 }
           ; 

class_decl
	: "class" TYPEID var_formals superclass '{' feature_list '}' 
		{ ClassDecl $2 (Just $4) $6 (T.toSymbol "filename") }
	;

superclass 
           : { T.toSymbol "Any" }
           | "extends" TYPEID actuals { if $2 == T.toSymbol "Any" then error "Bad! Cannot explicitly extend 'Any'" else $2 }
           | "extends" "native" { T.toSymbol "native" }
           ;

feature_list  : { [] }
              | feature_list feature { $2 : $1 }
              | feature_list '{' block '}' ';' { $1 ++ [$3] }
              ;

feature : opt_override DEF OBJECTID formals ':' TYPEID '=' expr ';' { Method $1 $3 $4 $6 $8 }
        | opt_override DEF OBJECTID formals ':' TYPEID '=' NATIVE ';' { Method $1 $3 $4 $6 (T.toExpr "no_expr") }
        | VAR OBJECTID ':' TYPEID '=' expr ';' { Attribute $2 $4 }
        | VAR OBJECTID '=' "native" ';' { Attribute $2 (T.toSymbol "Any") }
        ;

var_formals : '(' ')' { [] }
            | '(' var_formals_one_or_more ')' { $2 }
            ;

var_formals_one_or_more : var_formal { [$1] }
                        | var_formals_one_or_more ',' var_formal { $3 : $1 }
                        ;

var_formal : "var" OBJECTID ':' TYPEID { Formal $2 $4 }
           ;

formals : '('  ')' { [] }
        | '(' formals_one_or_more ')' { $2 }
        ;

formals_one_or_more : formal { [$1] }
                    | formals_one_or_more ',' formal { $3 : $1 }
                    ;

formal : OBJECTID ':' TYPEID { Formal $1 $3 }
       ;

expr   : OBJECTID '=' expr { Assign $1 $3 }
       | SUPER '.' OBJECTID actuals { StaticDispatch (Variable (T.toSymbol "this")) (T.toSymbol "superclass_name") $3 $4 }
       | expr '.' OBJECTID actuals { Dispatch $1 $3 $4 }
       | IF '(' expr ')' expr ELSE expr %prec IF { Cond $3 $5 $7 }
       | WHILE '(' expr ')' expr %prec IF { Loop $3 $5 }
       | '{' block '}' { $2 }
       | expr MATCH '{' simple_cases '}' { TypeCase $1 $4 }
       | NEW TYPEID actuals { Alloc $2 }
       | expr '+' expr { Add $1 $3 }
       | expr '-' expr { Sub $1 $3 }
       | expr '*' expr { Mul $1 $3 }
       | expr '/' expr { Div $1 $3 }
       | '-' expr %prec UNARY { Neg $2 }
       | expr '<' expr { Lt $1 $3 }
       | expr EQEQ expr { Leq $1 $3 }
       | expr LE expr { Leq $1 $3 }
       | '!' expr %prec UNARY { Comp $2 }
       | '(' expr ')' { $2 }
       | '(' ')' { Unit }
       | NULL { Nil }
       | INT_LIT { IntLit $1 }
       | STR_LIT { StringLit $1 }
       | BOOL_LIT { BoolLit $1 }
       | THIS { Variable (T.toSymbol "this") }
       | OBJECTID { Variable $1 }
       | OBJECTID actuals { Dispatch (Variable (T.toSymbol "this")) $1 $2 }
       ;

block : { Block [] }
      | stmt_list { Block $1 }
      ;

actuals : '(' ')' { [] }
        | '(' expr_list ')' { $2 }
        ;

expr_list : expr { [$1] }
          | expr_list ',' expr { $3 : $1 }
          ;

stmt_list : expr { [$1] }
          | expr ';' stmt_list { $3 ++ [$1] }
          | VAR OBJECTID ':' TYPEID '=' expr ';' stmt_list { [Let $2 $4 $6 (Block $8)] }
          ;

simple_cases : simple_case { [$1] }
             | simple_cases simple_case { $2 : $1 }
             ;

simple_case : CASE OBJECTID ':' TYPEID ARROW block { Branch $2 $4 $6 }
            | CASE NULL ARROW block { Branch (T.toSymbol "null") (T.toSymbol "Null") $4 }
            ; 

opt_override : OVERRIDE { True }
             | { False }
             ;
%%

parseError :: [T.Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
