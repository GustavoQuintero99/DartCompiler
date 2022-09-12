/* JFlex example: partial Java language lexer specification */
import java_cup.runtime.*;
import dart.compiler.DartReservedWords;
import dart.compiler.DartOperators;
import dart.compiler.DartBuiltInWords;
import com.sun.java_cup.internal.runtime.Symbol;
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
"="                            { return symbol(DartOperators.EQ); }
"=="                           { return symbol(DartOperators.EQEQ); }
"+"                            { return symbol(DartOperators.PLUS); }
"++"                           { return symbol(DartOperators.PLUSPLUS); }
"-"                            { return symbol(DartOperators.MINUS); }
"--"                           { return symbol(DartOperators.MINUSMINUS); }
"*"                            { return symbol(DartOperators.POW); }
"**"                           { return symbol(DartOperators.POWPOW); }
"&&"                           { return symbol(DartOperators.AND); }
"||"                           { return symbol(DartOperators.OR); }
"-="                           { return symbol(DartOperators.MINUSEQ); }
"+="                           { return symbol(DartOperators.PLUSEQ); }
"%"                            { return symbol(DartOperators.RESID); }

/* keywords */
<YYINITIAL> "assert"           {return symbol(DartReservedWords.ASSERT); }
<YYINITIAL> "break"            {return symbol(DartReservedWords.BREAK); }
<YYINITIAL> "case"             {return symbol(DartReservedWords.CASE); }
<YYINITIAL> "catch"            {return symbol(DartReservedWords.CATCH); }
<YYINITIAL> "class"            {return symbol(DartReservedWords.CLASS); }
<YYINITIAL> "const"            {return symbol(DartReservedWords.CONST); }
<YYINITIAL> "continue"         {return symbol(DartReservedWords.CONTINUE); }
<YYINITIAL> "default"          {return symbol(DartReservedWords.DEFAULT); }
<YYINITIAL> "do"               {return symbol(DartReservedWords.DO); }
<YYINITIAL> "else"             {return symbol(DartReservedWords.ELSE); }
<YYINITIAL> "enum"             {return symbol(DartReservedWords.ENUM); }
<YYINITIAL> "extends"          {return symbol(DartReservedWords.EXTENDS); }
<YYINITIAL> "false"            {return symbol(DartReservedWords.FALSE); }
<YYINITIAL> "final"            {return symbol(DartReservedWords.FINAL); }
<YYINITIAL> "finally"          {return symbol(DartReservedWords.FINALLY); }
<YYINITIAL> "for"              {return symbol(DartReservedWords.FOR); }
<YYINITIAL> "if"               {return symbol(DartReservedWords.IF); }
<YYINITIAL> "in"               {return symbol(DartReservedWords.IN); }
<YYINITIAL> "is"               {return symbol(DartReservedWords.IS); }
<YYINITIAL> "new"              {return symbol(DartReservedWords.NEW); }
<YYINITIAL> "null"             {return symbol(DartReservedWords.NULL); }
<YYINITIAL> "rethrow"          {return symbol(DartReservedWords.RETHROW); }
<YYINITIAL> "return"           {return symbol(DartReservedWords.RETURN); }
<YYINITIAL> "super"            {return symbol(DartReservedWords.SUPER); }
<YYINITIAL> "switch"           {return symbol(DartReservedWords.SWITCH); }
<YYINITIAL> "this"             {return symbol(DartReservedWords.THIS); }
<YYINITIAL> "throw"            {return symbol(DartReservedWords.THROW); }
<YYINITIAL> "true"             {return symbol(DartReservedWords.TRUE); }
<YYINITIAL> "try"              {return symbol(DartReservedWords.TRY); }
<YYINITIAL> "var"              {return symbol(DartReservedWords.VAR); }
<YYINITIAL> "void"             {return symbol(DartReservedWords.VOID); }
<YYINITIAL> "while"            {return symbol(DartReservedWords.WHILE); }
<YYINITIAL> "with"             {return symbol(DartReservedWords.WITH); }
<YYINITIAL> "await"            {return symbol(DartReservedWords.AWAIT); }
<YYINITIAL> "yield"            {return symbol(DartReservedWords.YIELD); }
<YYINITIAL> "async"            {return symbol(DartReservedWords.ASYNC); }
<YYINITIAL> "hide"             {return symbol(DartReservedWords.HIDE); }
<YYINITIAL> "on"               {return symbol(DartReservedWords.ON); }
<YYINITIAL> "show"             {return symbol(DartReservedWords.SHOW); }
<YYINITIAL> "sync"             {return symbol(DartReservedWords.SYNC); }
<YYINITIAL> "abstract"         {return symbol(DartBuiltInWords.ABSTRACT); }
<YYINITIAL> "as"               {return symbol(DartBuiltInWords.AS); }
<YYINITIAL> "covariant"        {return symbol(DartBuiltInWords.COVARIANT); }
<YYINITIAL> "deferred"         {return symbol(DartBuiltInWords.DEFERRED); }
<YYINITIAL> "dynamic"          {return symbol(DartBuiltInWords.DYNAMIC); }
<YYINITIAL> "export"           {return symbol(DartBuiltInWords.EXPORT); }
<YYINITIAL> "extension"        {return symbol(DartBuiltInWords.EXTENSION); }
<YYINITIAL> "external"         {return symbol(DartBuiltInWords.EXTERNAL); }
<YYINITIAL> "factory"          {return symbol(DartBuiltInWords.FACTORY); }
<YYINITIAL> "function"         {return symbol(DartBuiltInWords.FUNCTION); }
<YYINITIAL> "get"              {return symbol(DartBuiltInWords.GET); }
<YYINITIAL> "implements"       {return symbol(DartBuiltInWords.IMPLEMENTS); }
<YYINITIAL> "import"           {return symbol(DartBuiltInWords.IMPORT); }
<YYINITIAL> "interface"        {return symbol(DartBuiltInWords.INTERFACE); }
<YYINITIAL> "library"          {return symbol(DartBuiltInWords.LIBRARY); }
<YYINITIAL> "mixin"            {return symbol(DartBuiltInWords.MIXIN); }
<YYINITIAL> "operator"         {return symbol(DartBuiltInWords.OPERATOR); }
<YYINITIAL> "part"             {return symbol(DartBuiltInWords.PART); }
<YYINITIAL> "set"              {return symbol(DartBuiltInWords.SET); }
<YYINITIAL> "static"           {return symbol(DartBuiltInWords.STATIC); }
<YYINITIAL> "typedef"          {return symbol(DartBuiltInWords.TYPEDEF); }

<STRING> {
  \"                             { yybegin(YYINITIAL); 
                                   return symbol(sym.STRING_LITERAL, 
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