%{
#include "default-defs.h"
#include "decafcomp.tab.h"
#include <cstring>
#include <string>
#include <list>
#include <map>
#include <sstream>
#include <iostream>

using namespace std;

int lineno = 1;
int tokenpos = 1;
char *temp;
%}
/*regexp definition*/
letter [A-Za-z\_]+
decimal_digit [0-9]+
digit [0-9]+
hex_digit [A-Fa-f0-9]+
all_char [a-zA-Z0-9\a\b\t\v\f\r \!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~\n]+
char [a-zA-Z0-9\a\b\t\v\f\r \!\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\]\^\_\`\{\|\}\~]+
char_no_nl [a-zA-Z0-9\a\b\t\v\f\r \!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~]+
char_lit_chars [\a\b\t\v\f\r \!\"\#\$\%\&\(\)\*\+\,\-\.\/0-9\:\;\<\=\>\?\@A-Z\[\]\^\_\`a-z\{\|\}\~\n]
escaped_char \\(n|r|t|v|f|a|b|\\|\'|\")
nescaped_char \\[cdeghijklmopqsuwxyzA-Z0-9 \!\#\$\%\&\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\]\^\_\`\{\|\}\~]
hex_lit 0(x|X){hex_digit}+
decimal_lit {decimal_digit}+
%%
  /*
    Pattern definitions for all tokens
  */
package                     {tokenpos++;return T_PACKAGE;}
var     {tokenpos++;return T_VAR;}
while       {tokenpos++;return T_WHILE;}
for     {tokenpos++;return T_FOR;}
else    {tokenpos++;return T_ELSE;}
if      {tokenpos++;return T_IF;}
extern                      {tokenpos++;return T_EXTERN;}
func                       { tokenpos++;return T_FUNC; }
\(                         { tokenpos++;return T_LPAREN; }
\)                         { tokenpos++;return T_RPAREN; }
string      {tokenpos++;yylval.sval = new string("StringType");return T_STRINGTYPE;}
int                        { tokenpos++;yylval.sval = new string("IntType");return T_INTTYPE; }
bool                        {tokenpos++;yylval.sval = new string("BoolType");return T_BOOLTYPE;}
return      {tokenpos++;yylval.sval = new string(yytext);return T_RETURN;}
break   {tokenpos++;yylval.sval = new string(yytext);return T_BREAK;}
continue    {tokenpos++;yylval.sval = new string(yytext);return T_CONTINUE;}
{decimal_lit}    {tokenpos++;yylval.ival= atoi(yytext);return T_INTCONSTANT;}
{hex_lit}       {tokenpos++;yylval.ival= strtol(yytext,&temp,16);return T_INTCONSTANT;}
\'({char_lit_chars}|{escaped_char})\' {tokenpos++;yylval.cval=strdup(yytext);return T_CHARCONSTANT;}
\"({char}*{escaped_char}*|{escaped_char}*{char}*)*\"   {tokenpos++;yylval.sval = new string(yytext);return T_STRINGCONSTANT;}
\;  {tokenpos++;return T_SEMICOLON;}
\[  {tokenpos++;return T_LSB;}
\]  {tokenpos++;return T_RSB;}
\, {tokenpos++;return T_COMMA;}
\-  {tokenpos++;yylval.sval = new string("Minus");return T_MINUS;}
\!  {tokenpos++;yylval.sval = new string("Not");return T_NOT;}
\+  {tokenpos++;yylval.sval = new string("Plus");return T_PLUS;}
\*  {tokenpos++;yylval.sval = new string("Mult");return T_MULT;}
\/  {tokenpos++;yylval.sval = new string("Div");return T_DIV;}
\<\<    {tokenpos++;yylval.sval = new string("Leftshift");return T_LEFTSHIFT;}
\>\>    {tokenpos++;yylval.sval = new string("Rightshift");return T_RIGHTSHIFT;}
\<      {tokenpos++;yylval.sval = new string("Lt");return T_LT;}
\>      {tokenpos++;yylval.sval = new string("Rt");return T_GT;}
\%      {tokenpos++;yylval.sval = new string("Mod");return T_MOD;}
\<\=    {tokenpos++;yylval.sval = new string("Leq");return T_LEQ;}
\>\=    {tokenpos++;yylval.sval = new string("Geq");return T_GEQ;}
\=\=    {tokenpos++;yylval.sval = new string("Eq");return T_EQ;}
\!\=    {tokenpos++;yylval.sval = new string("Neq");return T_NEQ;}
\&\&                        {tokenpos++;yylval.sval = new string("And");return T_AND;}
\|\|    {tokenpos++;yylval.sval = new string("Or");return T_OR;}
\=                          {tokenpos++;return T_ASSIGN;}
true        {tokenpos++;yylval.sval = new string("True");return T_TRUE;}
false   {tokenpos++;yylval.sval = new string("False");return T_FALSE;}
void        {tokenpos++;yylval.sval = new string("VoidType");return T_VOID;}
\{                         { tokenpos++;return T_LCB; }
\}                         { tokenpos++;return T_RCB; }
[a-zA-Z\_][a-zA-Z\_0-9]*   { tokenpos++;yylval.sval = new string(yytext); return T_ID; } /* note that identifier pattern must be after all keywords */
[\t\r\a\v\b\f ]+           {}
[\n]+[\t\r\v\a\b\f ]*      {lineno++;tokenpos=1;}
\/\/{char_no_nl}\n          {lineno++;tokenpos=1;} /* ignore whitespace */
.                          { cerr << "Error: unexpected character in input" << endl; return -1; }

%%

int yyerror(const char *s) {
  cerr <<s << " at line "<<lineno <<", at char " << tokenpos << endl;
  return 1;
}

