%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
void yyerror(char *);
extern YYSTYPE yylval;
struct node* makenode(char nodetype,char* name,int value);
%}

letter [A-Za-z]
digit  [0-9]

%%
endwhile {return ENDWHILE;}
begin {return BEG;}
main {return MAIN;}
AND  {yylval.ptr=makenode('a',NULL,0);return AND;}
OR {yylval.ptr=makenode('o',NULL,0);return OR;}
NOT  {yylval.ptr=makenode('n',NULL,0);return NOT;}  
do       { return DO;}            
then      {return THEN;}
if       {yylval.ptr=makenode('I',NULL,0);return IF;}
else     {return ELSE;}
endif    {return ENDIF;}
return    {return RETURN;}
end   {return END;}
while  {yylval.ptr=makenode('W',NULL,0);return WHILE;}
decl     {return DECL;}
enddecl  {return ENDDECL;}
integer   {return INTEGER;}
boolean {return BOOLEAN;}
write  {yylval.ptr=makenode('w',NULL,0);return WRITE;}
read   {yylval.ptr=makenode('r',NULL,0);return READ;}
TRUE {yylval.ptr=makenode('b',NULL,1);return BIN;}
FALSE {yylval.ptr=makenode('b',NULL,0);return BIN;}
[0-9]+  {yylval.ptr=makenode('c',NULL,atoi(yytext));return NUMBER;}     
{letter}({letter}|{digit})*   {char *s=malloc(sizeof(yyleng));strcpy(s,yytext);yylval.ptr=makenode('v',s,0); return ID;}
">" {yylval.ptr=makenode('g',NULL,0);return RELOP;}
"<" {yylval.ptr=makenode('l',NULL,0);return RELOP;}
">=" {yylval.ptr=makenode('G',NULL,0);return RELOP;}
"<=" {yylval.ptr=makenode('L',NULL,0);return RELOP;}
"==" {yylval.ptr=makenode('e',NULL,0);return RELOP;}
"!=" {yylval.ptr=makenode('N',NULL,0);return RELOP;}
"+"       {yylval.ptr=makenode(*yytext,NULL,0);return PLUS;}
"-"       {yylval.ptr=makenode(*yytext,NULL,0);return MINUS;}
"*"       {yylval.ptr=makenode(*yytext,NULL,0);return MUL;}
"/"       {yylval.ptr=makenode(*yytext,NULL,0);return DIV;} 
"%"	  {yylval.ptr=makenode(*yytext,NULL,0);return REM;}
"="       {yylval.ptr=makenode(*yytext,NULL,0);return ASSI;}
"("   {return LBR;}
")"   {return RBR;}
"{"   {return LCB;}
"}"   {return RCB;} 
"["   {return LSB;}
"]"   {return RSB;}
";"   {return SEMCO;}
","   {return COMMA;}
[ \t\n]+ ;  /* White Spaces */
.     yyerror("Not Valid");
%%
int yywrap(void) {return 1;}

struct node* makenode(char nodetype,char* name,int value)
{
struct node* ptr1=malloc(sizeof(struct node));
ptr1->name=name;
ptr1->nodetype=nodetype;
ptr1->c1=NULL;
ptr1->c2=NULL;
ptr1->c3=NULL;
ptr1->value=value;
ptr1->type=-1;	
return ptr1;
}

