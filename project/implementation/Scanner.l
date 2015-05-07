D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%{

#include <string>
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include "Scanner.h"
 
#define STOKEN( x ) ( new std::string( x ) ) 
#define yyterminate() return( token::END ) 
#define YY_NO_UNISTD_H

typedef intergdb::lang::Parser::token token; 

%}
 
%option debug 
%option nodefault 
%option yyclass="Scanner" 
%option noyywrap 
%option c++
 
%%

Conf  { return(token::CONF); }
name  { return(token::NAME); }
path  { return(token::PATH); }
schema { return(token::SCHEMA); }
vertex { return(token::VERTEX); }
edge { return(token::EDGE); }

STRING { return(token::TYPE_STRING); }
INT64 { return(token::TYPE_INT64); }
DOUBLE { return(token::TYPE_DOUBLE); }

Graph  { return(token::GRAPH); }
using  { return(token::USING); }

Select { return(token::SELECT); }
exists { return(token::EXISTS); }

all { return(token::ALL); }
from { return(token::FROM); }

">" { return(token::GT); }
">=" { return(token::GTE); }
"<" { return(token::LT); }
"<=" { return(token::LTE); }
"==" { return(token::EQ); }
"!=" { return(token::NEQ); }

"&&" { return(token::AND); }
"||" { return(token::OR); }
"!" { return(token::NOT); }

"{" { return(token::LEFT_BRACE); }
"}" { return(token::RIGHT_BRACE); }
"(" { return(token::LEFT_PAREN); }
")" { return(token::RIGHT_PAREN); }
":" { return(token::COLON); }
"," { return(token::COMMA); }

"-" { return(token::MINUS); }
"+" { return(token::PLUS); }
"*" { return(token::STAR); }

"/*"			        { comment(); }

{L}({L}|{D})*	 	    { yylval->sval = STOKEN( yytext ); return( token::IDENTIFIER); }

0[xX]{H}+{IS}?		    { yylval->sval = STOKEN( yytext ); return( token::CONSTANT); }
0{D}+{IS}?	   	        { yylval->sval = STOKEN( yytext ); return( token::CONSTANT); }
{D}+{IS}?		        { yylval->sval = STOKEN( yytext ); return( token::CONSTANT_INT); }
L?'(\\.|[^\\'])+' 	    { yylval->sval = STOKEN( yytext ); return( token::CONSTANT); }

{D}+{E}{FS}?		    { yylval->sval = STOKEN( yytext ); return( token::CONSTANT_DOUBLE); }
{D}*"."{D}+({E})?{FS}?	{ yylval->sval = STOKEN( yytext ); return( token::CONSTANT_DOUBLE); }
{D}+"."{D}*({E})?{FS}?	{ yylval->sval = STOKEN( yytext ); return( token::CONSTANT_DOUBLE); }

L?\"(\\.|[^\\"])*\"	    { yylval->sval = STOKEN( yytext ); return(token::STRING_LITERAL); }

"->"			        { return(token::RIGHT_ARROW); }




[ \t\v\n\f]	   	        { /* ignore whitespace */ }
.		  	            { /* ignore bad characters */ }


%%


void 
intergdb::lang::Scanner::comment()
{
  char c, c1;
       
   loop:
       while ((c = yyinput()) != '*' && c != 0)
           ;
       
       if ((c1 = yyinput()) != '/' && c != 0)
       {
           unput(c1);
           goto loop;
       }
       
       if (c != 0)
           ;
}
