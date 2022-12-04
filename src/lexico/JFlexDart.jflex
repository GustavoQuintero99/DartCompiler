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
|"!"|"==" )                    { return symbol(sym.RELATIONAL_OPERATOR, yytext());}

"="                            { return symbol(sym.EQ, yytext()); }
"=="                           { return symbol(sym.EQEQ, yytext()); }
"+"                            { return symbol(sym.PLUS, yytext()); }
"/"                            { return symbol(sym.DIVISION, yytext()); }
"++"                           { return symbol(sym.PLUSPLUS, yytext()); }
"-"                            { return symbol(sym.MINUS, yytext()); }
"--"                           { return symbol(sym.MINUSMINUS, yytext()); }
"*"                            { return symbol(sym.POW, yytext()); }
"**"                           { return symbol(sym.POWPOW, yytext()); }
"&&"                           { return symbol(sym.AND, yytext()); }
"||"                           { return symbol(sym.OR, yytext()); }
"-="                           { return symbol(sym.MINUSEQ, yytext()); }
"+="                           { return symbol(sym.PLUSEQ, yytext()); }
"%"                            { return symbol(sym.RESID, yytext()); }
" "                            { }
"{"                            { return symbol(sym.OPENING_KEY, yytext()); }
"}"                            { return symbol(sym.CLOSING_KEY, yytext()); }
"("                            { return symbol(sym.OPENING_PARENT, yytext()); }
")"                            { return symbol(sym.CLOSING_PARENT, yytext()); }
";"                            { return symbol(sym.SEMICOLON, yytext());}
":"                            { return symbol(sym.COLON, yytext());}
"."                            { return symbol(sym.DOT, yytext());}
"\["                           { return symbol(sym.OPENING_BRACKET, yytext()); }
"\]"                           { return symbol(sym.CLOSING_BRACKET, yytext()); }
","                            { return symbol(sym.COMMA, yytext()); }

"true"|"false"                   { return symbol(sym.BOOLEAN_OPERATOR, yytext()); }



/* keywords */
<YYINITIAL>("var"|"int"|"double")         {return symbol(sym.DATA_TYPE,yytext()); }
<YYINITIAL>"var"                          {return symbol(sym.VAR, yytext()); }
<YYINITIAL>"int"                          {return symbol(sym.INT, yytext()); }
<YYINITIAL>"double"                       {return symbol(sym.DOUBLE, yytext()); }
<YYINITIAL>"bool"                         {return symbol(sym.BOOL, yytext()); }
<YYINITIAL>"String"                       {return symbol(sym.STRING, yytext()); }


<YYINITIAL> "assert"           {return symbol(sym.ASSERT, yytext()); }
<YYINITIAL> "break"            {return symbol(sym.BREAK, yytext()); }
<YYINITIAL> "case"             {return symbol(sym.CASE, yytext()); }
<YYINITIAL> "catch"            {return symbol(sym.CATCH, yytext()); }
<YYINITIAL> "class"            {return symbol(sym.CLASS, yytext()); }
<YYINITIAL> "const"            {return symbol(sym.CONST, yytext()); }
<YYINITIAL> "continue"         {return symbol(sym.CONTINUE, yytext()); }
<YYINITIAL> "default"          {return symbol(sym.DEFAULT, yytext()); }
<YYINITIAL> "do"               {return symbol(sym.DO, yytext()); }
<YYINITIAL> "else"             {return symbol(sym.ELSE, yytext()); }
<YYINITIAL> "enum"             {return symbol(sym.ENUM, yytext()); }
<YYINITIAL> "extends"          {return symbol(sym.EXTENDS, yytext()); }
<YYINITIAL> "false"            {return symbol(sym.FALSE, yytext()); }
<YYINITIAL> "final"            {return symbol(sym.FINAL, yytext()); }
<YYINITIAL> "finally"          {return symbol(sym.FINALLY, yytext()); }
<YYINITIAL> "for"              {return symbol(sym.FOR, yytext()); }
<YYINITIAL> "if"               {return symbol(sym.IF, yytext()); }
<YYINITIAL> "else if"               {return symbol(sym.IF, yytext()); }

<YYINITIAL> "in"               {return symbol(sym.IN, yytext()); }
<YYINITIAL> "is"               {return symbol(sym.IS, yytext()); }
<YYINITIAL> "new"              {return symbol(sym.NEW, yytext()); }
<YYINITIAL> "null"             {return symbol(sym.NULL, yytext()); }
<YYINITIAL> "rethrow"          {return symbol(sym.RETHROW, yytext()); }
<YYINITIAL> "return"           {return symbol(sym.RETURN, yytext()); }
<YYINITIAL> "super"            {return symbol(sym.SUPER, yytext()); }
<YYINITIAL> "switch"           {return symbol(sym.SWITCH, yytext()); }
<YYINITIAL> "this"             {return symbol(sym.THIS, yytext()); }
<YYINITIAL> "throw"            {return symbol(sym.THROW, yytext()); }
<YYINITIAL> "true"             {return symbol(sym.TRUE, yytext()); }
<YYINITIAL> "try"              {return symbol(sym.TRY, yytext()); }
<YYINITIAL> "var"              {return symbol(sym.VAR, yytext()); }
<YYINITIAL> "void"             {return symbol(sym.VOID, yytext()); }
<YYINITIAL> "while"            {return symbol(sym.WHILE, yytext()); }
<YYINITIAL> "with"             {return symbol(sym.WITH, yytext()); }
<YYINITIAL> "await"            {return symbol(sym.AWAIT, yytext()); }
<YYINITIAL> "yield"            {return symbol(sym.YIELD, yytext()); }
<YYINITIAL> "async"            {return symbol(sym.ASYNC, yytext()); }
<YYINITIAL> "hide"             {return symbol(sym.HIDE, yytext()); }
<YYINITIAL> "on"               {return symbol(sym.ON, yytext()); }
<YYINITIAL> "show"             {return symbol(sym.SHOW, yytext()); }
<YYINITIAL> "sync"             {return symbol(sym.SYNC, yytext()); }
<YYINITIAL> "main"             {return symbol(sym.MAIN, yytext()); }
<YYINITIAL> "print"            {return symbol(sym.PRINT, yytext()); }
<YYINITIAL> "abstract"         {return symbol(sym.ABSTRACT, yytext()); }
<YYINITIAL> "as"               {return symbol(sym.AS, yytext()); }
<YYINITIAL> "covariant"        {return symbol(sym.COVARIANT, yytext()); }
<YYINITIAL> "deferred"         {return symbol(sym.DEFERRED, yytext()); }
<YYINITIAL> "dynamic"          {return symbol(sym.DYNAMIC, yytext()); }
<YYINITIAL> "export"           {return symbol(sym.EXPORT, yytext()); }
<YYINITIAL> "extension"        {return symbol(sym.EXTENSION, yytext()); }
<YYINITIAL> "external"         {return symbol(sym.EXTERNAL, yytext()); }
<YYINITIAL> "factory"          {return symbol(sym.FACTORY, yytext()); }
<YYINITIAL> "function"         {return symbol(sym.FUNCTION, yytext()); }
<YYINITIAL> "get"              {return symbol(sym.GET, yytext()); }
<YYINITIAL> "implements"       {return symbol(sym.IMPLEMENTS, yytext()); }
<YYINITIAL> "import"           {return symbol(sym.IMPORT, yytext()); }
<YYINITIAL> "interface"        {return symbol(sym.INTERFACE, yytext()); }
<YYINITIAL> "library"          {return symbol(sym.LIBRARY, yytext()); }
<YYINITIAL> "mixin"            {return symbol(sym.MIXIN, yytext()); }
<YYINITIAL> "operator"         {return symbol(sym.OPERATOR, yytext()); }
<YYINITIAL> "part"             {return symbol(sym.PART, yytext()); }
<YYINITIAL> "set"              {return symbol(sym.SET, yytext()); }
<YYINITIAL> "static"           {return symbol(sym.STATIC, yytext()); }
<YYINITIAL> "typedef"          {return symbol(sym.TYPEDEF, yytext()); }

 <YYINITIAL> {
    /* Comments */
    {Comment}               { /* ignore */}

    /* WhiteSpace */ 
    {WhiteSpace}            { /*ignore*/}
    
     /* Identificador */
    {Identifier}    { return symbol(sym.IDENTIFIER, yytext());  }

    /* Numeros */
    {DecIntegerLiteral}    { return symbol(sym.DECINTEGERLITERAL, yytext()); }
    \"                             { string.setLength(0); yybegin(STRING); }


 }
    <STRING> {
      \"                             { yybegin(YYINITIAL); 
                                       return symbol(sym.STRINGLITERAL, 
                                       string.toString()); }
      [^\n\r\"\\]+                   { string.append( yytext() ); }
      \\t                            { string.append('\t'); }
      \\n                            { string.append('\n'); }

      \\r                            { string.append('\r'); }
      \\\"                           { string.append('\"'); }
      \\                             { string.append('\\'); }
      \'                             { string.append('\''); }
      

    }

/* error fallback */
[^]                              { throw new Error("Illegal character <"+
                                                    yytext()+">"); }