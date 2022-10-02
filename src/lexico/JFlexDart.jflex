/* JFlex example: partial Java language lexer specification */
package lexico;


import java_cup.runtime.*;
import lexico.DartReservedWords;
import lexico.DartOperators;
import lexico.DartBuiltInWords;
import java_cup.sym;
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
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} | {DocumentationComment}

TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
// Comment can be the last line of the file, without line terminator.
EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/**" {CommentContent} "*"+ "/"
CommentContent       = ( [^*] | \*+ [^/*] )*

Identifier = [:jletter:] [:jletterdigit:]*

DecIntegerLiteral = 0 | [1-9][0-9]*

%state STRING

%%

/* ops */
"="                            { return symbol(DartOperators.EQ.value, yytext()); }
"=="                           { return symbol(DartOperators.EQEQ.value, yytext()); }
"+"                            { return symbol(DartOperators.PLUS.value, yytext()); }
"++"                           { return symbol(DartOperators.PLUSPLUS.value, yytext()); }
"-"                            { return symbol(DartOperators.MINUS.value, yytext()); }
"--"                           { return symbol(DartOperators.MINUSMINUS.value, yytext()); }
"*"                            { return symbol(DartOperators.POW.value, yytext()); }
"**"                           { return symbol(DartOperators.POWPOW.value, yytext()); }
"&&"                           { return symbol(DartOperators.AND.value, yytext()); }
"||"                           { return symbol(DartOperators.OR.value, yytext()); }
"-="                           { return symbol(DartOperators.MINUSEQ.value, yytext()); }
"+="                           { return symbol(DartOperators.PLUSEQ.value, yytext()); }
"%"                            { return symbol(DartOperators.RESID.value, yytext()); }
" "                            { return symbol(DartOperators.SPACE.value, yytext()); }
"{"                            { return symbol(DartOperators.OPENCODE.value, yytext()); }
"}"                            { return symbol(DartOperators.CLOSECODE.value, yytext()); }
"("                            { return symbol(DartOperators.OPENPARENT.value, yytext()); }
")"                            { return symbol(DartOperators.CLOSEPARENT.value, yytext()); }
"\n"                           { return symbol(DartOperators.LINEABREAK.value, yytext());}
";"                            { return symbol(DartOperators.SEMICOLON.value, yytext());}

<YYINITIAL> "assert"           {return symbol(DartReservedWords.ASSERT.value, yytext()); }
<YYINITIAL> "break"            {return symbol(DartReservedWords.BREAK.value, yytext()); }
<YYINITIAL> "case"             {return symbol(DartReservedWords.CASE.value, yytext()); }
<YYINITIAL> "catch"            {return symbol(DartReservedWords.CATCH.value, yytext()); }
<YYINITIAL> "class"            {return symbol(DartReservedWords.CLASS.value, yytext()); }
<YYINITIAL> "const"            {return symbol(DartReservedWords.CONST.value, yytext()); }
<YYINITIAL> "continue"         {return symbol(DartReservedWords.CONTINUE.value, yytext()); }
<YYINITIAL> "default"          {return symbol(DartReservedWords.DEFAULT.value, yytext()); }
<YYINITIAL> "do"               {return symbol(DartReservedWords.DO.value, yytext()); }
<YYINITIAL> "else"             {return symbol(DartReservedWords.ELSE.value, yytext()); }
<YYINITIAL> "enum"             {return symbol(DartReservedWords.ENUM.value, yytext()); }
<YYINITIAL> "extends"          {return symbol(DartReservedWords.EXTENDS.value, yytext()); }
<YYINITIAL> "false"            {return symbol(DartReservedWords.FALSE.value, yytext()); }
<YYINITIAL> "final"            {return symbol(DartReservedWords.FINAL.value, yytext()); }
<YYINITIAL> "finally"          {return symbol(DartReservedWords.FINALLY.value, yytext()); }
<YYINITIAL> "for"              {return symbol(DartReservedWords.FOR.value, yytext()); }
<YYINITIAL> "if"               {return symbol(DartReservedWords.IF.value, yytext()); }
<YYINITIAL> "in"               {return symbol(DartReservedWords.IN.value, yytext()); }
<YYINITIAL> "is"               {return symbol(DartReservedWords.IS.value, yytext()); }
<YYINITIAL> "new"              {return symbol(DartReservedWords.NEW.value, yytext()); }
<YYINITIAL> "null"             {return symbol(DartReservedWords.NULL.value, yytext()); }
<YYINITIAL> "rethrow"          {return symbol(DartReservedWords.RETHROW.value, yytext()); }
<YYINITIAL> "return"           {return symbol(DartReservedWords.RETURN.value, yytext()); }
<YYINITIAL> "super"            {return symbol(DartReservedWords.SUPER.value, yytext()); }
<YYINITIAL> "switch"           {return symbol(DartReservedWords.SWITCH.value, yytext()); }
<YYINITIAL> "this"             {return symbol(DartReservedWords.THIS.value, yytext()); }
<YYINITIAL> "throw"            {return symbol(DartReservedWords.THROW.value, yytext()); }
<YYINITIAL> "true"             {return symbol(DartReservedWords.TRUE.value, yytext()); }
<YYINITIAL> "try"              {return symbol(DartReservedWords.TRY.value, yytext()); }
<YYINITIAL> "var"              {return symbol(DartReservedWords.VAR.value, yytext()); }
<YYINITIAL> "void"             {return symbol(DartReservedWords.VOID.value, yytext()); }
<YYINITIAL> "while"            {return symbol(DartReservedWords.WHILE.value, yytext()); }
<YYINITIAL> "with"             {return symbol(DartReservedWords.WITH.value, yytext()); }
<YYINITIAL> "await"            {return symbol(DartReservedWords.AWAIT.value, yytext()); }
<YYINITIAL> "yield"            {return symbol(DartReservedWords.YIELD.value, yytext()); }
<YYINITIAL> "async"            {return symbol(DartReservedWords.ASYNC.value, yytext()); }
<YYINITIAL> "hide"             {return symbol(DartReservedWords.HIDE.value, yytext()); }
<YYINITIAL> "on"               {return symbol(DartReservedWords.ON.value, yytext()); }
<YYINITIAL> "show"             {return symbol(DartReservedWords.SHOW.value, yytext()); }
<YYINITIAL> "sync"             {return symbol(DartReservedWords.SYNC.value, yytext()); }
<YYINITIAL> "main"             {return symbol(DartReservedWords.MAIN.value, yytext()); }
<YYINITIAL> "print"            {return symbol(DartReservedWords.PRINT.value, yytext()); }
<YYINITIAL> "abstract"         {return symbol(DartBuiltInWords.ABSTRACT.value, yytext()); }
<YYINITIAL> "as"               {return symbol(DartBuiltInWords.AS.value, yytext()); }
<YYINITIAL> "covariant"        {return symbol(DartBuiltInWords.COVARIANT.value, yytext()); }
<YYINITIAL> "deferred"         {return symbol(DartBuiltInWords.DEFERRED.value, yytext()); }
<YYINITIAL> "dynamic"          {return symbol(DartBuiltInWords.DYNAMIC.value, yytext()); }
<YYINITIAL> "export"           {return symbol(DartBuiltInWords.EXPORT.value, yytext()); }
<YYINITIAL> "extension"        {return symbol(DartBuiltInWords.EXTENSION.value, yytext()); }
<YYINITIAL> "external"         {return symbol(DartBuiltInWords.EXTERNAL.value, yytext()); }
<YYINITIAL> "factory"          {return symbol(DartBuiltInWords.FACTORY.value, yytext()); }
<YYINITIAL> "function"         {return symbol(DartBuiltInWords.FUNCTION.value, yytext()); }
<YYINITIAL> "get"              {return symbol(DartBuiltInWords.GET.value, yytext()); }
<YYINITIAL> "implements"       {return symbol(DartBuiltInWords.IMPLEMENTS.value, yytext()); }
<YYINITIAL> "import"           {return symbol(DartBuiltInWords.IMPORT.value, yytext()); }
<YYINITIAL> "interface"        {return symbol(DartBuiltInWords.INTERFACE.value, yytext()); }
<YYINITIAL> "library"          {return symbol(DartBuiltInWords.LIBRARY.value, yytext()); }
<YYINITIAL> "mixin"            {return symbol(DartBuiltInWords.MIXIN.value, yytext()); }
<YYINITIAL> "operator"         {return symbol(DartBuiltInWords.OPERATOR.value, yytext()); }
<YYINITIAL> "part"             {return symbol(DartBuiltInWords.PART.value, yytext()); }
<YYINITIAL> "set"              {return symbol(DartBuiltInWords.SET.value, yytext()); }
<YYINITIAL> "static"           {return symbol(DartBuiltInWords.STATIC.value, yytext()); }
<YYINITIAL> "typedef"          {return symbol(DartBuiltInWords.TYPEDEF.value, yytext()); }
<YYINITIAL> (\"(\\.|[^\"]+)*\") {return symbol(DartOperators.STRINGLITERAL.value, yytext()); }

<STRING> {
  \"                             { yybegin(YYINITIAL); 
                                   return symbol(DartOperators.STRINGLITERAL.value, 
                                   string.toString()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }

  \\r                            { string.append('\r'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
}

/* error fallback */
[^]                              { throw new Error("Illegal character <"+
                                                    yytext()+">"); }