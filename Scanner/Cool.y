/* Cool.flex language lexer specification */

%%

%class CoolScanner
%type CoolTokens.YYToken
%implements Iterator[CoolTokens.YYToken]

%line


%{
  // These features are added to the Scanner class

  // You will find this attribute useful for building up strings.
  // See StringBuffer.cool
  var string_error : String = null;
  var string : StringBuffer = new StringBuffer();
  def symbol(n : String) : Symbol = Symbol(n);

  var in_basic_file : Boolean = false;
  def set_in_basic_file(v : Boolean) : Unit = in_basic_file = v;
  
  var io : IO = new IO();
  var lookahead : CoolTokens.YYToken = null;
   
  override def hasNext() : Boolean = { 
    if (io.is_null(lookahead)) lookahead = yylex();
    lookahead match {
      case x:CoolTokens.YYEOF => false;
      case x:CoolTokens.YYToken => true;
    }
  };
  
  override def next() : CoolTokens.YYToken = {
    if (io.is_null(lookahead)) lookahead = yylex();
    var result : CoolTokens.YYToken = lookahead;
    lookahead = null;
    result
  };
  
  def getLineNumber() : Int = yyline+1;

%}

/*
 * Define names for regular expressions here.
 */

%state STRING
%state TRIPLE_STRING

NEWLINE = [\n]
WHITESPACE = [ \t\r]


%%

<YYINITIAL> {
  // ignore these
  {NEWLINE}  { }
  {WHITESPACE}  { }


 /*
  *  The multiple-character operators.
  *
  * You need to use "return" for all tokens.
  * Otherwise it will continue scanning (which is the correct
  * thing to do after seeing whitespace or a comment).
*/

 "--".*[\r\n\v]     { }
 [\n\t\r\v]+   { }
 "//".*         {}
 /* "**".*[\r\n\v]?     { } */
 "\\(\\*"(.*|\r|\n\v)*"\\*\\)"   { }
 // "\\(\\*"(.*|[\r\n\v])*"\\*\\)"   { }
 "<="		{ return new CoolTokens.LE(); }
 "<"    { return new CoolTokens.YYCHAR('<'); }
 "//"   {  }   
 "=="   { return new CoolTokens.EQEQ(); }
 "=>"   { return new CoolTokens.ARROW(); }
 "("    { return new CoolTokens.YYCHAR('('); }
 ")"    { return new CoolTokens.YYCHAR(')'); }
 "{"    { return new CoolTokens.YYCHAR('{'); }
 "}"    { return new CoolTokens.YYCHAR('}'); }
 "["    { return new CoolTokens.YYCHAR('['); }
 "]"    { return new CoolTokens.YYCHAR(']'); }
 ":"    { return new CoolTokens.YYCHAR(':'); }
 ";"    { return new CoolTokens.YYCHAR(';'); }
 "="    { return new CoolTokens.YYCHAR('='); }
 "."    { return new CoolTokens.YYCHAR('.'); }
 ","    { return new CoolTokens.YYCHAR(','); }
 // binary infix operators but do not have token in CoolTokens.scala
 "+"    { return new CoolTokens.YYCHAR('+'); }
 "/"    { return new CoolTokens.YYCHAR('/'); }
 "*"    { return new CoolTokens.YYCHAR('*'); }
 "-"    { return new CoolTokens.YYCHAR('-'); }
 // is a UNARY Operator according to Cool Manual 10.4
 // "."    { return new CoolTokens.UNARY(); }
 "@"    { return new CoolTokens.UNARY(); }
 "not"  { return new CoolTokens.UNARY(); }
 "isvoid" { return new CoolTokens.UNARY(); }
 "<-"   { return new CoolTokens.UNARY(); }
 
 // NOT ALLOWED, should throw errors
 ">"  { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 ">=" { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "|"  { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "||" { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "**" { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "%"  { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 ">>" { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "<<" { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "^"  { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }
 "!"  { return new CoolTokens.ERROR("Illegal Operator:" + yytext()); }

 /*
  * Keywords are all lowercase.  The keyword 
  * "native" is only allowed if in_basic_file is true.
  * (This condition must be checked here.)
  */

  "if"   { return new CoolTokens.IF(); }
  "else" { return new CoolTokens.ELSE(); }
  "def"  { return new CoolTokens.DEF();  }
  "class"  { return new CoolTokens.CLASS(); }
  "case" { return new CoolTokens.CASE(); }
  "override" { return new CoolTokens.OVERRIDE(); }
  "var"  { return new CoolTokens.VAR();  }
  "while"  { return new CoolTokens.WHILE(); }
  "this" { return new CoolTokens.THIS(); }
  "super"  { return new CoolTokens.SUPER(); }
  "native" { return new CoolTokens.NATIVE(); }
  "extends" { return new CoolTokens.EXTENDS(); }
  "match" { return new CoolTokens.MATCH(); }
  "null"  { return new CoolTokens.NULL(); }
  "new"   { return new CoolTokens.NEW(); }
  "true"    { return new CoolTokens.BOOL_LIT(true); }
  "false" { return new CoolTokens.BOOL_LIT(false); }
  // Should have tokens but do not in (CoolTokens.scala)
  /* "fi"    { return new CoolTokens.FI(); }
  "inherits" { return new CoolTokens.INHERITS(); }
  "let"   { return new CoolTokens.LET(); }
  "loop"  { return new CoolTokens.LOOP(); }
  "pool"  { return new CoolTokens.POOL(); }
  "then"  { return new CoolTokens.THEN(); }
  "esac"  { return new CoolTokens.ESAC(); }
  "of"    { return new CoolTokens.OF(); } */

 /*
  * The illegal keywords (see manual)
  */
  "object"    { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "abstract"  { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "yield"     { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "val"       { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "private"   { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "trait"     { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "with"      { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "None"      { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "Exception" { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "to"        { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "import"    { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "type"      { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }
  "package"   { return new CoolTokens.ERROR("Illegal Keyword:" + yytext()); }


		  
  /*
   * Various kinds of identifiers:
   *   objectIDs
   *   TypeIDs
   *   _illegalIDs
   */
   [a-z][a-zA-Z_]*      { return new CoolTokens.OBJECTID(symbol(yytext())); }
   [A-Z][a-zA-Z]*       { return new CoolTokens.TYPEID(symbol(yytext())); }
   _[a-zA-Z]+           { return new CoolTokens.ERROR("Bad Illegal ID" + yytext()); }
   // [0-9][a-zA-Z_0-9]*   { return new CoolTokens.ERROR("ID starting with a digit: " + yytext()); }
   // [a-zA-Z0-9_]*        { return new CoolTokens.ERROR("ID containing a digit: " + yytext()); }
   0|[1-9][0-9]*        { return new CoolTokens.INT_LIT(symbol(yytext())); }

		  
 /*
  *  String literals: "..."
  *  Escape sequence \c is accepted for some characters c.
  *  \n \t \b \f \r \\ \" are legal.
  *  Anything else is illegal.
  *  This is easiest to handle with a new start state.
  * Also handle the form """ anything at all """
  */

"\"\"\""    { string.setLength(0); yybegin(TRIPLE_STRING)}
"\""      { string.setLength(0); yybegin(STRING); }


 <<EOF>>	{ return new CoolTokens.YYEOF(); }

}

<TRIPLE_STRING> {
        "\"\"\""  { yybegin(YYINITIAL); 
                    var result = string.toString();
                    string.setLength(0);
                    return new CoolTokens.STR_LIT(symbol(result));
                  }
        // shown in Office Hours is equivalent to states
        /* "\"\"\""([^\"]|\"[^\"]|\"\"[^\"])* "\"\"\"" {
          var str : String = yytext();
          return new CoolTokens.STR_LIT(symbol(str.substring(3, str.length - 3))); } */

     .  {   string.append(yytext());  }

 <<EOF>>	{ yybegin(YYINITIAL); return new CoolTokens.ERROR("saw EOF while tokenizing triple quotes"); }
}

<STRING> {
        "\"" { yybegin(YYINITIAL); return new CoolTokens.STR_LIT(symbol(string.toString()));}
        "\\n"  { string.append('\n'); }
        "\\t"  { string.append('\t'); }
        "\\b"  { string.append('\b'); }
        "\\f"  { string.append('\f'); }
        "\\r"  { string.append('\r'); }
        "\\"   { string.append('\\'); }
        "\\\"" { string.append("\""); }
        "\\\\" { string.append("\\"); }

        "\\"[.]   { yybegin(YYINITIAL); return new CoolTokens.ERROR("Backlash followed by " + yytext() + " is illegal"); }
        "\\"\n  { yybegin(YYINITIAL); return new CoolTokens.ERROR("Newlines cannot be followed by" + yytext()); }
        // Can never be matched according to Output
        // \\      { yybegin(YYINITIAL); return new CoolTokens.ERROR("Backslash Error" + yytext()); }
        \n      { yybegin(YYINITIAL); return new CoolTokens.ERROR("Unterminated string" + yytext()); }
        .    { string.append(yytext()); }
        <<EOF>> { yybegin(YYINITIAL); return new CoolTokens.ERROR("Null Ending Character during tokenization"); }
}

  [^]		{ new IO().out("Error: scanner leaked character: ").out(yytext()).out("\n"); }




