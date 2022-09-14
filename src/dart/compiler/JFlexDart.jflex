/* JFlex example: partial Java language lexer specification */
import java_cup.runtime.*;
import dart.compiler.DartReservedWords;
import dart.compiler.DartOperators;
import dart.compiler.DartBuiltInWords;
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
"="                            { return symbol(DartOperators.EQ.value); }
"=="                           { return symbol(DartOperators.EQEQ.value); }
"+"                            { return symbol(DartOperators.PLUS.value); }
"++"                           { return symbol(DartOperators.PLUSPLUS.value); }
"-"                            { return symbol(DartOperators.MINUS.value); }
"--"                           { return symbol(DartOperators.MINUSMINUS.value); }
"*"                            { return symbol(DartOperators.POW.value); }
"**"                           { return symbol(DartOperators.POWPOW.value); }
"&&"                           { return symbol(DartOperators.AND.value); }
"||"                           { return symbol(DartOperators.OR.value); }
"-="                           { return symbol(DartOperators.MINUSEQ.value); }
"+="                           { return symbol(DartOperators.PLUSEQ.value); }
"%"                            { return symbol(DartOperators.RESID.value); }

<YYINITIAL> "assert"           {return symbol(DartReservedWords.ASSERT.value); }
<YYINITIAL> "break"            {return symbol(DartReservedWords.BREAK.value); }
<YYINITIAL> "case"             {return symbol(DartReservedWords.CASE.value); }
<YYINITIAL> "catch"            {return symbol(DartReservedWords.CATCH.value); }
<YYINITIAL> "class"            {return symbol(DartReservedWords.CLASS.value); }
<YYINITIAL> "const"            {return symbol(DartReservedWords.CONST.value); }
<YYINITIAL> "continue"         {return symbol(DartReservedWords.CONTINUE.value); }
<YYINITIAL> "default"          {return symbol(DartReservedWords.DEFAULT.value); }
<YYINITIAL> "do"               {return symbol(DartReservedWords.DO.value); }
<YYINITIAL> "else"             {return symbol(DartReservedWords.ELSE.value); }
<YYINITIAL> "enum"             {return symbol(DartReservedWords.ENUM.value); }
<YYINITIAL> "extends"          {return symbol(DartReservedWords.EXTENDS.value); }
<YYINITIAL> "false"            {return symbol(DartReservedWords.FALSE.value); }
<YYINITIAL> "final"            {return symbol(DartReservedWords.FINAL.value); }
<YYINITIAL> "finally"          {return symbol(DartReservedWords.FINALLY.value); }
<YYINITIAL> "for"              {return symbol(DartReservedWords.FOR.value); }
<YYINITIAL> "if"               {return symbol(DartReservedWords.IF.value); }
<YYINITIAL> "in"               {return symbol(DartReservedWords.IN.value); }
<YYINITIAL> "is"               {return symbol(DartReservedWords.IS.value); }
<YYINITIAL> "new"              {return symbol(DartReservedWords.NEW.value); }
<YYINITIAL> "null"             {return symbol(DartReservedWords.NULL.value); }
<YYINITIAL> "rethrow"          {return symbol(DartReservedWords.RETHROW.value); }
<YYINITIAL> "return"           {return symbol(DartReservedWords.RETURN.value); }
<YYINITIAL> "super"            {return symbol(DartReservedWords.SUPER.value); }
<YYINITIAL> "switch"           {return symbol(DartReservedWords.SWITCH.value); }
<YYINITIAL> "this"             {return symbol(DartReservedWords.THIS.value); }
<YYINITIAL> "throw"            {return symbol(DartReservedWords.THROW.value); }
<YYINITIAL> "true"             {return symbol(DartReservedWords.TRUE.value); }
<YYINITIAL> "try"              {return symbol(DartReservedWords.TRY.value); }
<YYINITIAL> "var"              {return symbol(DartReservedWords.VAR.value); }
<YYINITIAL> "void"             {return symbol(DartReservedWords.VOID.value); }
<YYINITIAL> "while"            {return symbol(DartReservedWords.WHILE.value); }
<YYINITIAL> "with"             {return symbol(DartReservedWords.WITH.value); }
<YYINITIAL> "await"            {return symbol(DartReservedWords.AWAIT.value); }
<YYINITIAL> "yield"            {return symbol(DartReservedWords.YIELD.value); }
<YYINITIAL> "async"            {return symbol(DartReservedWords.ASYNC.value); }
<YYINITIAL> "hide"             {return symbol(DartReservedWords.HIDE.value); }
<YYINITIAL> "on"               {return symbol(DartReservedWords.ON.value); }
<YYINITIAL> "show"             {return symbol(DartReservedWords.SHOW.value); }
<YYINITIAL> "sync"             {return symbol(DartReservedWords.SYNC.value); }
<YYINITIAL> "abstract"         {return symbol(DartBuiltInWords.ABSTRACT.value); }
<YYINITIAL> "as"               {return symbol(DartBuiltInWords.AS.value); }
<YYINITIAL> "covariant"        {return symbol(DartBuiltInWords.COVARIANT.value); }
<YYINITIAL> "deferred"         {return symbol(DartBuiltInWords.DEFERRED.value); }
<YYINITIAL> "dynamic"          {return symbol(DartBuiltInWords.DYNAMIC.value); }
<YYINITIAL> "export"           {return symbol(DartBuiltInWords.EXPORT.value); }
<YYINITIAL> "extension"        {return symbol(DartBuiltInWords.EXTENSION.value); }
<YYINITIAL> "external"         {return symbol(DartBuiltInWords.EXTERNAL.value); }
<YYINITIAL> "factory"          {return symbol(DartBuiltInWords.FACTORY.value); }
<YYINITIAL> "function"         {return symbol(DartBuiltInWords.FUNCTION.value); }
<YYINITIAL> "get"              {return symbol(DartBuiltInWords.GET.value); }
<YYINITIAL> "implements"       {return symbol(DartBuiltInWords.IMPLEMENTS.value); }
<YYINITIAL> "import"           {return symbol(DartBuiltInWords.IMPORT.value); }
<YYINITIAL> "interface"        {return symbol(DartBuiltInWords.INTERFACE.value); }
<YYINITIAL> "library"          {return symbol(DartBuiltInWords.LIBRARY.value); }
<YYINITIAL> "mixin"            {return symbol(DartBuiltInWords.MIXIN.value); }
<YYINITIAL> "operator"         {return symbol(DartBuiltInWords.OPERATOR.value); }
<YYINITIAL> "part"             {return symbol(DartBuiltInWords.PART.value); }
<YYINITIAL> "set"              {return symbol(DartBuiltInWords.SET.value); }
<YYINITIAL> "static"           {return symbol(DartBuiltInWords.STATIC.value); }
<YYINITIAL> "typedef"          {return symbol(DartBuiltInWords.TYPEDEF.value); }


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