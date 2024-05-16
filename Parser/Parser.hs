{
module Parser.Parser where
import Data.Char

import Data.Maybe
import Data.List

import qualified Scanner.Tokens as T
import Parser.AST
import Parser.TerminalNode
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
      '+'             { T.PLUS{} }
      '-'             { T.MINUS {} }
      '*'             { T.TIMES {} }
      '/'             { T.DIVIDE {} }
      '<'             { T.LT {} }
      '<='            { T.LE {} }
      'new'           { T.NEW {} }
      '('             { T.LPAREN {} }
      ')'             { T.RPAREN {} }
      '{'             { T.LBRACE {} }
      '}'             { T.RBRACE {} }
      ':'             { T.COLON {} }
      '='             { T.ASSIGN {} }
      ';'             { T.SEMICOLON {} }
      ','             { T.COMMA {} }
      '.'             { T.PERIOD {} }
      '!'             { T.NOT {} }
      'class'         { T.CLASS {} }
      'extends'       { T.EXTENDS {} }
      'case'          { T.CASE {} }
      '=>'            { T.ARROW {} }
      'if'            { T.IF {} }
      'then'          { T.THEN {} }
      'else'          { T.ELSE {} }
      OBJECTID        { T.OBJECTID {} }
      TYPEID          { T.TYPEID {} }
      'true'          { T.TRUE {} }
      'false'         { T.FALSE {} }
      'while'         { T.WHILE {} }

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

program : class_list { result = result.concat($1); }

program :: { Program }
program : class ';' { Program [$1] }
        | program class ';'{ Program $ ((getClasses $1) ++ [$2])}

class :: { Class }
class :
        'class' typeID '{' feats '}'    { Class (T.getName $2) "Object" $4 }
      | 'class' typeID  'inherits' typeID '{' feats '}'    { Class (T.getName $2) (T.getName $4) $6 }

features :: { [Feature] }
features :
        {- empty -}               { [] }
      | feats feat ';'     { $1 ++ [$2] }

feature :: { Feature }
feature :
        objectID ':' typeID opt_expr        { Attribute (T.getName $1) (T.getName $3) $4 }
      | objectID
        '(' optMethodParameters ')'
        ':' typeID
        '{' expr '}'                        { Method (T.getName $1) $3 (T.getName $6) $8}

formal :: { Formal }
formal :
          objectID ':' typeID               { Formal (T.getName $1) (T.getName $3) }

formals :: { [Formal] }
formals :
               formal                        { [ $1 ] }
             | methodParameters ',' formal   { $1 ++ [$3]}

optMethodParameters :
                {- empty -}              { [] }
              | methodParameters         { $1 }


opt_expr :: {Maybe Expression}
opt_expr : {- empty -}            { Nothing }
         | '<-' expr              { Just $2 }

exprs :: { [Expression] }
exprs : expr ';'                  { [$1] }
      | exprs expr ';'            { $1 ++ [$2] }

letBinding :: { LetBinding }
letBinding :
             objectID ':' typeID ',' letBinding           { LetDeclaration (T.getName $1) (T.getName $3) Nothing $5 }
           | objectID ':' typeID '<-' expr ',' letBinding { LetDeclaration (T.getName $1) (T.getName $3) (Just $5) $7 }
           | objectID ':' typeID 'in' expr                { LetBinding (T.getName $1) (T.getName $3) Nothing $5 }
           | objectID ':' typeID '<-' expr 'in' expr      { LetBinding (T.getName $1) (T.getName $3) (Just $5) $7 }

caseBranch :: { CaseBranch }
caseBranch :
            objectID ':' typeID  '=>' expr ';' {CaseBranch (T.getName $1) (T.getName $3) $5 }

caseBranches :: { [CaseBranch] }
caseBranches :
                caseBranch               { [ $1 ]}
              | caseBranches caseBranch  { $1 ++ [$2] }

methodInputs :: { [Expression] }
methodInputs :
                expr                    { [ $1 ] }
              | methodInputs ',' expr   { $1 ++ [$3]}

optMethodInputs :: { [Expression] }
optMethodInputs :
              {- empty -}               { [] }
            | methodInputs              { $1 }

staticDispatch :: { Expression }
staticDispatch :
        expr '@' typeID '.'
              objectID '('
              optMethodInputs
              ')'                     { StaticMethodDispatch $1 (T.getName $3) (T.getName $5) $7 }
      | expr '.'
            objectID '('
            optMethodInputs
            ')'                       { MethodDispatch $1 (T.getName $3) $5 }

expr :: { Expression }
expr  :

        objectID '<-' expr      { AssignmentExpr (T.getName $1) $3 }
      | staticDispatch          { $1 }
      | objectID '('
        optMethodInputs
        ')'                     { MethodDispatch SelfVarExpr (T.getName $1) $3 }
      | 'if' expr 'then'
        expr 'else' expr 'fi'   { CondExpr $2 $4 $6 }
      | 'while' expr
        'loop' expr
        'pool'                  { LoopExpr $2 $4 }
      | '{' exprs '}'           { BlockExpr $2 }
      | 'let' letBinding        { LetExpr $2 }
      | 'case' expr 'of'
         caseBranches 'esac'    { TypeCaseExpr $2 $4 }
      | 'new' typeID            { NewExpr (T.getName $2) }
      | 'isvoid' expr           { IsvoidExpr $2 }
      | expr '+' expr           { PlusExpr $1 $3 }
      | expr '-' expr           { MinusExpr $1 $3 }
      | expr '*' expr           { TimesExpr $1 $3 }
      | expr '/' expr           { DivideExpr $1 $3 }
      | '~' expr                { NegExpr $2 }
      | expr '<' expr           { LessThanExpr $1 $3 }
      | expr '<=' expr          { LessThanOrEqualExpr $1 $3 }
      | expr '=' expr           { EqualExpr $1 $3 }
      | 'not' expr              { NotExpr $2 }
      | '(' expr ')'            { $2 }
      | objectID                { IdentifierExpr (T.getName $1) }
      | int                     { IntegerExpr (T.getValue $1) }
      | string                  { StringExpr (T.getStrVal $1)}
      | 'true'                  { TrueExpr }
      | 'false'                 { FalseExpr }

{
parseError :: [T.Token] -> a
parseError tokens = error ("Parse error: Remaining Tokens " ++  (intercalate ";" (map show tokens)))

}