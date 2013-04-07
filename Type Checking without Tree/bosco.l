%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
void yyerror(char *);
extern YYSTYPE yylval;
struct node* makenode(char* name,int value);
%}

letter [A-Za-z]
digit  [0-9]

%%
endwhile {return ENDWHILE;}
begin {return BEG;}
main {return MAIN;}
AND  {return AND;}
OR {return OR;}
NOT  {return NOT;}  
do       { return DO;}            
then      {return THEN;}
if       {return IF;}
else     {return ELSE;}
endif    {return ENDIF;}
return    {return RETURN;}
end   {return END;}
while  {return WHILE;}
decl     {return DECL;}
enddecl  {return ENDDECL;}
integer   {return INTEGER;}
boolean {return BOOLEAN;}
write  {return WRITE;}
read   {return READ;}
TRUE {return BIN;}
FALSE {return BIN;}
[0-9]+  {yylval.ptr=makenode(NULL,atoi(yytext));return NUMBER;}     
{letter}({letter}|{digit})*   {char *s=malloc(sizeof(yyleng));strcpy(s,yytext);yylval.ptr=makenode(s,1); return ID;}
">" {return RELOP;}
"<" {return RELOP;}
">=" {return RELOP;}
"<=" {return RELOP;}
"==" {return RELOP;}
"!=" {return RELOP;}
"+"       {return PLUS;}
"-"       {return MINUS;}
"*"       {return MUL;}
"/"       {return DIV;} 
"%"	  {return REM;}
"="       {return ASSI;}
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
void yyerror(char *s)
{
printf("\n\n%s\n\n",s);
exit(0);
}

struct node* makenode(char* name,int value)
{
struct node *ptr1=malloc(sizeof(struct node));
ptr1->name=name;
ptr1->c1=NULL;
ptr1->c2=NULL;
ptr1->c3=NULL;
ptr1->value=value;
ptr1->type=0;
return ptr1;
}

