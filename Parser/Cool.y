/*
 *  cool.y
 *              Parser definition for the COOL language.
 */

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.
*/
%token CASE CLASS DEF ELSE EXTENDS IF
%token MATCH NATIVE NEW NULL OVERRIDE SUPER THIS VAR WHILE
%token <Symbol>  STR_LIT INT_LIT 
%token <Boolean> BOOL_LIT
%token <Symbol>  TYPEID OBJECTID
%token EQEQ LE ARROW 
%token <String> ERROR

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/

  /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See the bison documentation for details. */

/* Declare types for the grammar's non-terminals. */
%type <Classes> class_list
%type <Class> class_decl
%type <Symbol> superclass
%type <Features> feature_list
%type <Feature> feature
%type <Formals> var_formals formals var_formals_one_or_more formals_one_or_more
%type <Formal> formal var_formal
%type <Expression> expr block
%type <Expressions> actuals expr_list stmt_list
%type <Cases> simple_cases
%type <Case> simple_case
%type <Boolean> opt_override

/* Precedence Declarations */
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
        ;
        
class_list : class_decl { $$ = new Classes_one($1); }
           | class_list  class_decl { $$ = new Classes_append($1, new Classes_one($2)); }
           ; 

class_decl
	: CLASS TYPEID var_formals superclass '{' feature_list '}'
		{ $$ = class_decl($2,$4,
				  make_constructor($2,$3).concat($6),
				  symbol(filename)); }
	;


superclass : /* empty */ { superclass_name = symbol("Any"); $$ = superclass_name; add_supercall(superclass_name,new Expressions_nil()); }
	       | EXTENDS TYPEID actuals { superclass_name = $2; if (superclass_name == symbol("Any")) { yyerror("Bad! Cannot explicitly extend 'Any'"); } else {}; $$ = superclass_name; add_supercall($2,$3); }
	       | EXTENDS NATIVE { superclass_name = Symbol("native"); $$ = superclass_name; native_constructor(); }
	       ;


// SR and RR conflict
feature_list  : /* empty feature list */ { $$ = new Features_nil(); }
              | feature_list feature { $$ = new Features_append($1, new Features_one($2)); }
              // | feature { $$ = new Features_one($1); }
              | feature_list '{' block '}' ';' { add_to_constructor($3); $$ = $1; }
;

// RR in Case 1
feature : opt_override DEF OBJECTID formals ':' TYPEID '=' expr ';' { $$ = method($1,$3,$4,$6,$8); }
	      | opt_override DEF OBJECTID formals ':' TYPEID '=' NATIVE ';' { $$ = method($1,$3,$4,$6,no_expr()); }
	      | VAR OBJECTID ':' TYPEID '=' expr ';' { $$ = attr($2,$4); add_to_constructor(attr_init($2,$6)); }
	      | VAR OBJECTID '=' NATIVE ';' { $$ = attr($2,symbol("Any")); current_inherit_status = false;  native_constructor(); }
	      ;

var_formals : '(' ')' { $$ = new Formals_nil(); }
            | '(' var_formals_one_or_more ')' { $$ = $2 }
            ;

var_formals_one_or_more :
          var_formal { $$ = new Formals_one($1); }
        | var_formals_one_or_more ',' var_formal { $$ = new Formals_append($1, new Formals_one($3)); };

var_formal : VAR OBJECTID ':' TYPEID { $$ = formal($2, $4); };

formals : '('  ')' { $$ = new Formals_nil(); }
        | '(' formals_one_or_more ')' { $$ = $2; }
        ;

formals_one_or_more : formal { $$ = new Formals_one($1) }
        | formals_one_or_more ',' formal { $$ = new Formals_append($1, new Formals_one($3)); }
        ;

formal : OBJECTID ':' TYPEID { $$ = formal($1, $3); };

expr   : OBJECTID '=' expr                  { $$ = assign($1 ,$3) }
       | SUPER '.' OBJECTID actuals         
	  { val this_obj : Expression = variable(symbol("this"));
	    $$ = static_dispatch(this_obj,superclass_name,$3,$4); }
       | expr '.' OBJECTID actuals          { $$ = dispatch($1, $3, $4); }
       | IF '(' expr ')' expr ELSE expr %prec IF { $$ = cond($3, $5, $7) }
       | WHILE '(' expr ')' expr %prec IF   { $$ = loop($3, $5) }
       | '{' block '}'                      { $$ = $2; }
       | expr MATCH '{' simple_cases '}'       { $$ = typecase($1, $4) }
       | NEW TYPEID actuals                 { $$ = dispatch(alloc($2),$2,$3) }
       | expr '+' expr                      { $$ =  add($1, $3); }
       | expr '-' expr                      { $$ =  sub($1, $3); }
       | expr '*' expr                      { $$ =  mul($1, $3); }
       | expr '/' expr                      { $$ =  div($1, $3); }
       | '-' expr %prec UNARY               { $$ = neg($2); }
       | expr '<' expr                      { $$ =  lt($1, $3); }
       | expr EQEQ expr                     { $$ =  leq($1, $3); }
       | expr LE expr                       { $$ = leq($1, $3); }
       | '!' expr %prec UNARY               { $$ = comp($2) }
       | '(' expr ')'                       { $$ = $2 }
       | '(' ')'                            { $$ =  unit(); }
       | NULL                               { $$ = nil() }
       | INT_LIT                            { $$ =  int_lit($1); }
       | STR_LIT                            { $$ =  string_lit($1); }
       | BOOL_LIT                           { $$ =  bool_lit($1); }
       | THIS                               { $$ =  variable( symbol("this")); }
       | OBJECTID                           { $$ =  variable($1); }
       | OBJECTID actuals                   { $$ = implicit_dispatch(variable(symbol("this")),$1,$2);  }
       ;

block : /* EMPTY */ { $$ = block(new Expressions_nil()); }
      | stmt_list {$$ = block($1) }
      ;

actuals : 
    '(' ')'  { $$ = new Expressions_nil(); }
  | '(' expr_list ')' { $$ = $2; }
        ;

expr_list : expr { $$ = new Expressions_one($1); }
          | expr_list ',' expr { $$ = new Expressions_append($1, new Expressions_one($3)); }
          ;

stmt_list : expr    { $$ = new Expressions_one($1); }
          | expr  ';' stmt_list   { $$ = new Expressions_append(new Expressions_one($1), $3); }   
          | VAR OBJECTID ':' TYPEID  '=' expr ';' stmt_list { $$ = new Expressions_one(let($2,$4,$6,block($8))); }
          ;

/* 1 or more case */
simple_cases : simple_case { $$ = new Cases_one($1); }
      | simple_cases simple_case { $$ = new Cases_append($1, new Cases_one($2)); }
      ;

simple_case : CASE OBJECTID ':' TYPEID ARROW block { $$ = branch($2, $4, $6); }
            | CASE NULL ARROW block { $$ = branch(symbol("null"), symbol("Null"), $4); }
            ; 

opt_override : OVERRIDE { $$ = true; }
             | {$$ = false}
             ;
/* end of grammar */
%%
/************************************************************************/
/*                DON'T CHANGE ANYTHING IN THIS SECTION                 */
/************************************************************************/

// Features all added automatically to the parser class

var scanner : CoolScanner = null;
var filename : String = "<unknown>";
var num_errors : Int = 0;
var result : Classes = new Classes_nil();
var superclass_name : Symbol = null;
var current_inherit_status : Boolean = true;
var io : IO = new IO();
var options : CoolOptions = new CoolOptions();

def get_result() : Classes = result;

def set_options(co : CoolOptions) : Unit = {
  options = co;
  yydebug = co.get_parse_debug()
};

def reset(sc : CoolScanner, fn : String) : Unit = {
  filename = fn;
  scanner = sc;
  num_errors = 0;
  result = new Classes_nil();
  superclass_name = null;
  current_inherit_status = true;

  yyreset(sc)
};

def symbol(name : String) : Symbol = io.symbol(name);

// Customizing the node factory
override def get_line_number() : Int = scanner.getLineNumber();

// we override the class_decl factory method to use the 
// variable "current_inherit_status" AND 
// to reset it afterwards for the next class.
override def class_decl(name:Symbol,parent:Symbol,features:Features,filename:Symbol) : Cclass_decl = {
  var result : Cclass_decl = super.class_decl(name,parent,features,filename);
  result.set_inheritablep(current_inherit_status);
  current_inherit_status = true;
  result
};


// Code to help build constructors:
var constr_is_native : Boolean = false;
var constr_body : Expressions = new Expressions_nil();
var attr_parameters : Features = new Features_nil();

def add_to_constructor(e : Expression) : Unit = { 
  constr_body = constr_body.addcopy(e)
};

def attr_parameter(name : Symbol, typename : Symbol) : Formal = {
  var mod_name : Symbol = symbol(name.toString());
  attr_parameters = attr_parameters.addcopy(attr(name,typename));
  add_to_constructor(assign(name,variable(mod_name)));
  formal(mod_name,typename);
};

def add_supercall(supername : Symbol, actuals : Expressions) : Unit = {
  add_to_constructor(static_dispatch(variable(symbol("this")),
				     supername,supername,actuals))
};

def native_constructor() : Unit = {
  constr_is_native = true
};

def make_constructor(name : Symbol, formals : Formals) : Features = {
  // Create the constructor and return a list of features
  // including all the parameter attributes and the constructor.
  // If the constructor is native, then the body should be no_expr()
  // otherwise it should be a block of the pieces collected so far
  // ending with "this" (to return the newly initialized object).
  //
  // Before returning, the side-effected variables need to be reset:
  // - constr_is_native
  // - constr_body
  // - attr_parameters
  // This will give the next class a clean slate for its own constructor.
  var body : Expressions = constr_body.addcopy(variable(symbol("this")));
  var constr : Feature = 
    method(false,name,formals,name,
	   if (constr_is_native) no_expr() 
	     else block(body));
  var result : Features = attr_parameters.addcopy(constr);
  constr_is_native = false;
  constr_body = new Expressions_nil();
  attr_parameters = new Features_nil();
  result
};

def eq(e1:Expression,e2:Expression) = super.dispatch(e1,symbol("equals"),new Expressions_one(e2));
def implicit_dispatch(e : Expression, s : Symbol, a : Expressions) : Expression
  = super.dispatch(e,s,a);
def attr_init(s : Symbol, e : Expression) : Expression = super.assign(s,e);

/* This function is called automatically when Bison detects a parse error. */
def yyerror(message : String) : Unit = {
  io.out(filename).out(":").out_any(scanner.getLineNumber()).out(": ");
  io.out(message).out(", at or near ").out_any(yycur).out("\n");

  num_errors = num_errors + 1;
  var max : Int = options.get_max_errors();
  if (max < num_errors) io.abort("More than ".concat(max.toString()).concat(" errors")) else ()
};

def get_errors() : Int = num_errors;
