/* JFlex example: partial Java language lexer specification */
package lexico;


import java_cup.runtime.*;
import lexico.sym;
/**
 * Dart Lexer
 */
%%

%class Lexer
%unicode
%cup
%line
%column

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
}
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn);
}
  private Symbol symbol(int type, int type1, int type3, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
}
%}

 LineTerminator = \r|\n|\r\n
 InputCharacter = [^\r\n]
 WhiteSpace = {LineTerminator} | [ \t\f]
 /* comments */
 Comment = {TraditionalComment} | {EndOfLineComment} | {DocumentationComment}
 TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
 EndOfLineComment = "//" {InputCharacter}* {LineTerminator}
 DocumentationComment = "/**" {CommentContent} "*"+ "/"
 CommentContent = ( [^*] | \*+ [^/*] )*
 Identifier = [:jletter:] [:jletterdigit:]*
 DecIntegerLiteral = 0 | [1-9][0-9]*
 WhiteSpace = [\n\r\t ]
 


%state STRING

%%

/* ops */
("\<"|"\>"|"\<="|"\>="|"!="
|"!"|"==" )                    { return symbol(sym.RELATIONAL_OPERATOR, yychar, yyline, yytext());}

"="                            { return symbol(sym.EQ, yychar, yyline, yytext());}
"=="                           { return symbol(sym.EQEQ, yychar, yyline, yytext());}
"++"                           { return symbol(sym.PLUSPLUS, yychar, yyline, yytext());}
"--"                           { return symbol(sym.MINUSMINUS, yychar, yyline, yytext());}
"**"                           { return symbol(sym.POWPOW, yychar, yyline, yytext());}
"&&"                           { return symbol(sym.AND, yychar, yyline, yytext());}
"||"                           { return symbol(sym.OR, yychar, yyline, yytext());}
"-="                           { return symbol(sym.MINUSEQ, yychar, yyline, yytext());}
"+="                           { return symbol(sym.PLUSEQ, yychar, yyline, yytext());}
"%"                            { return symbol(sym.RESID, yychar, yyline, yytext());}
" "                            {}
"{"                            { return symbol(sym.OPENING_KEY, yychar, yyline, yytext());}
"}"                            { return symbol(sym.CLOSING_KEY, yychar, yyline, yytext());}
"("                            { return symbol(sym.OPENING_PARENT, yychar, yyline, yytext());}
")"                            { return symbol(sym.CLOSING_PARENT, yychar, yyline, yytext());}
";"                            { return symbol(sym.SEMICOLON, yychar, yyline, yytext());}
":"                            { return symbol(sym.COLON, yychar, yyline, yytext());}
"."                            { return symbol(sym.DOT, yychar, yyline, yytext());}
"\["                           { return symbol(sym.OPENING_BRACKET, yychar, yyline, yytext());}
"\]"                           { return symbol(sym.CLOSING_BRACKET, yychar, yyline, yytext());}
","                            { return symbol(sym.COMMA, yychar, yyline, yytext());}

"-"                            { return symbol(sym.MINUS, yychar, yyline, yytext());}
"+"                            { return symbol(sym.PLUS, yychar, yyline, yytext());}
"/"                            { return symbol(sym.DIVISION, yychar, yyline, yytext());}
"*"                           { return symbol(sym.MULTIPLICATION, yychar, yyline, yytext());}

"true"|"false"                   { return symbol(sym.BOOLEAN_OPERATOR, yychar, yyline, yytext());}



/* keywords */
<YYINITIAL>("var"|"int"|"double")         {return symbol(sym.DATA_TYPE, yychar, yyline, yytext());}
<YYINITIAL>"var"                          {return symbol(sym.VAR, yychar, yyline, yytext());}
<YYINITIAL>"int"                          {return symbol(sym.INT, yychar, yyline, yytext());}
<YYINITIAL>"double"                       {return symbol(sym.DOUBLE, yychar, yyline, yytext());}
<YYINITIAL>"bool"                         {return symbol(sym.BOOL, yychar, yyline, yytext());}
<YYINITIAL>"String"                       {return symbol(sym.STRING, yychar, yyline, yytext());}


<YYINITIAL> "assert"           {return symbol(sym.ASSERT, yychar, yyline, yytext());}
<YYINITIAL> "break"            {return symbol(sym.BREAK, yychar, yyline, yytext());}
<YYINITIAL> "case"             {return symbol(sym.CASE, yychar, yyline, yytext());}
<YYINITIAL> "catch"            {return symbol(sym.CATCH, yychar, yyline, yytext());}
<YYINITIAL> "class"            {return symbol(sym.CLASS, yychar, yyline, yytext());}
<YYINITIAL> "const"            {return symbol(sym.CONST, yychar, yyline, yytext());}
<YYINITIAL> "continue"         {return symbol(sym.CONTINUE, yychar, yyline, yytext());}
<YYINITIAL> "default"          {return symbol(sym.DEFAULT, yychar, yyline, yytext());}
<YYINITIAL> "do"               {return symbol(sym.DO, yychar, yyline, yytext());}
<YYINITIAL> "else"             {return symbol(sym.ELSE, yychar, yyline, yytext());}
<YYINITIAL> "enum"             {return symbol(sym.ENUM, yychar, yyline, yytext());}
<YYINITIAL> "extends"          {return symbol(sym.EXTENDS, yychar, yyline, yytext());}
<YYINITIAL> "false"            {return symbol(sym.FALSE, yychar, yyline, yytext());}
<YYINITIAL> "final"            {return symbol(sym.FINAL, yychar, yyline, yytext());}
<YYINITIAL> "finally"          {return symbol(sym.FINALLY, yychar, yyline, yytext());}
<YYINITIAL> "for"              {return symbol(sym.FOR, yychar, yyline, yytext());}
<YYINITIAL> "if"               {return symbol(sym.IF, yychar, yyline, yytext());}
<YYINITIAL> "else if"               {return symbol(sym.IF, yychar, yyline, yytext());}

<YYINITIAL> "in"               {return symbol(sym.IN, yychar, yyline, yytext());}
<YYINITIAL> "is"               {return symbol(sym.IS, yychar, yyline, yytext());}
<YYINITIAL> "new"              {return symbol(sym.NEW, yychar, yyline, yytext());}
<YYINITIAL> "null"             {return symbol(sym.NULL, yychar, yyline, yytext());}
<YYINITIAL> "rethrow"          {return symbol(sym.RETHROW, yychar, yyline, yytext());}
<YYINITIAL> "return"           {return symbol(sym.RETURN, yychar, yyline, yytext());}
<YYINITIAL> "super"            {return symbol(sym.SUPER, yychar, yyline, yytext());}
<YYINITIAL> "switch"           {return symbol(sym.SWITCH, yychar, yyline, yytext());}
<YYINITIAL> "this"             {return symbol(sym.THIS, yychar, yyline, yytext());}
<YYINITIAL> "throw"            {return symbol(sym.THROW, yychar, yyline, yytext());}
<YYINITIAL> "true"             {return symbol(sym.TRUE, yychar, yyline, yytext());}
<YYINITIAL> "try"              {return symbol(sym.TRY, yychar, yyline, yytext());}
<YYINITIAL> "var"              {return symbol(sym.VAR, yychar, yyline, yytext());}
<YYINITIAL> "void"             {return symbol(sym.VOID, yychar, yyline, yytext());}
<YYINITIAL> "while"            {return symbol(sym.WHILE, yychar, yyline, yytext());}
<YYINITIAL> "with"             {return symbol(sym.WITH, yychar, yyline, yytext());}
<YYINITIAL> "await"            {return symbol(sym.AWAIT, yychar, yyline, yytext());}
<YYINITIAL> "yield"            {return symbol(sym.YIELD, yychar, yyline, yytext());}
<YYINITIAL> "async"            {return symbol(sym.ASYNC, yychar, yyline, yytext());}
<YYINITIAL> "hide"             {return symbol(sym.HIDE, yychar, yyline, yytext());}
<YYINITIAL> "on"               {return symbol(sym.ON, yychar, yyline, yytext());}
<YYINITIAL> "show"             {return symbol(sym.SHOW, yychar, yyline, yytext());}
<YYINITIAL> "sync"             {return symbol(sym.SYNC, yychar, yyline, yytext());}
<YYINITIAL> "main"             {return symbol(sym.MAIN, yychar, yyline, yytext());}
<YYINITIAL> "print"            {return symbol(sym.PRINT, yychar, yyline, yytext());}
<YYINITIAL> "abstract"         {return symbol(sym.ABSTRACT, yychar, yyline, yytext());}
<YYINITIAL> "as"               {return symbol(sym.AS, yychar, yyline, yytext());}
<YYINITIAL> "covariant"        {return symbol(sym.COVARIANT, yychar, yyline, yytext());}
<YYINITIAL> "deferred"         {return symbol(sym.DEFERRED, yychar, yyline, yytext());}
<YYINITIAL> "dynamic"          {return symbol(sym.DYNAMIC, yychar, yyline, yytext());}
<YYINITIAL> "export"           {return symbol(sym.EXPORT, yychar, yyline, yytext());}
<YYINITIAL> "extension"        {return symbol(sym.EXTENSION, yychar, yyline, yytext());}
<YYINITIAL> "external"         {return symbol(sym.EXTERNAL, yychar, yyline, yytext());}
<YYINITIAL> "factory"          {return symbol(sym.FACTORY, yychar, yyline, yytext());}
<YYINITIAL> "function"         {return symbol(sym.FUNCTION, yychar, yyline, yytext());}
<YYINITIAL> "get"              {return symbol(sym.GET, yychar, yyline, yytext());}
<YYINITIAL> "implements"       {return symbol(sym.IMPLEMENTS, yychar, yyline, yytext());}
<YYINITIAL> "import"           {return symbol(sym.IMPORT, yychar, yyline, yytext());}
<YYINITIAL> "interface"        {return symbol(sym.INTERFACE, yychar, yyline, yytext());}
<YYINITIAL> "library"          {return symbol(sym.LIBRARY, yychar, yyline, yytext());}
<YYINITIAL> "mixin"            {return symbol(sym.MIXIN, yychar, yyline, yytext());}
<YYINITIAL> "operator"         {return symbol(sym.OPERATOR, yychar, yyline, yytext());}
<YYINITIAL> "part"             {return symbol(sym.PART, yychar, yyline, yytext());}
<YYINITIAL> "set"              {return symbol(sym.SET, yychar, yyline, yytext());}
<YYINITIAL> "static"           {return symbol(sym.STATIC, yychar, yyline, yytext());}
<YYINITIAL> "typedef"          {return symbol(sym.TYPEDEF, yychar, yyline, yytext());}

 <YYINITIAL> {
    /* Comments */
    {Comment}               { /* ignore */}

    /* WhiteSpace */ 
    {WhiteSpace}            { /*ignore*/}
    
     /* Identificador */
    {Identifier}    { return symbol(sym.IDENTIFIER, yychar, yyline, yytext());}

    /* Numeros */
    {DecIntegerLiteral}    { return symbol(sym.DECINTEGERLITERAL, yychar, yyline, yytext());}
    \"                             { string.setLength(0); yybegin(STRING);}


}
    <STRING> {
      \"                             { yybegin(YYINITIAL); 
                                       return symbol(sym.STRINGLITERAL, 
                                       string.toString());}
      [^\n\r\"\\]+                   { string.append( yytext() );}
      \\t                            { string.append('\t');}
      \\n                            { string.append('\n');}

      \\r                            { string.append('\r');}
      \\\"                           { string.append('\"');}
      \\                             { string.append('\\');}
      \'                             { string.append('\'');}
      

  }

/* error fallback */
[^]                              { throw new Error("Illegal character <"+
                                                    yytext()+">");}