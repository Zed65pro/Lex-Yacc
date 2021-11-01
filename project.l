%{
#include "y.tab.h"
void yyerror(char* s);
//[0-9]*(\.[0-9]+)? {yylval.f = atof(yytext); return number; }
%}



%%
"main=1"      { return start_command; }
"main=0"      { return exit_command; }
"output"     {return PRINT;}
"-help" return tok_help;
"int"  {return tok_int;}
"float" {return tok_float;}
"char" return tok_char;
"string" return tok_string;

"tan" return TAN;
"cos" return COS;
"sin" return COS;
"log" return LOG;

"is" return IS;
"oris" return ORIS;
"or" return OR;
"do" return DO;
"end" return END;
"aslong" return ASLONG;
"events" return EVENTS;
"event" return EVENT;
"events=0" return END_EVENTS;
"repeat" return REPEAT;
"until" return UNTIL;

[0]     { yylval.d = atoi(yytext); return number; }
[1-9][0-9]*      { yylval.d = atoi(yytext); return number; }
[0][.][0-9]+ {yylval.f = atof(yytext); return FLOAT;}
[1-9]*[.][0-9]+ {yylval.f = atof(yytext); return FLOAT;}


['][a-zA-Z1-9]['] {yylval.c=yytext[1]; return character;}

["][a-zA-Z1-9 ]+["] {
      int j=0;
      for(int i=0;i<strlen(yytext);++i)
        if(yytext[i]!='"'){

          yylval.str[j]=yytext[i];
          ++j;
          }
      yylval.str[j]='\0';

      return STR;
  }

">=" return great_eq;
"<=" return less_eq;
"==" return eq;
"!=" return not_eq;


[a-zA-Z][a-zA-Z0-9_]*    {
        yylval.id=strdup(yytext);
        return identifier;
        }

[ \t\n]+     ;
"\r\n" ;
[-+*/()^%;?:.] { return yytext[0]; }
[<>=!&|]    { return yytext[0]; }
.       { ECHO; yyerror("Unexpected character"); }
%%

int yywrap(void) { return 1; }
