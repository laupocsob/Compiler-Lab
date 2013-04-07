%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int vtype;
struct gtable
{
char *name;
int type;
int size;
struct gtable *next;
}*ghead;
struct gtable* glookup(char *name);
void ginstall(char *name,int type,int size);

struct ltable
{
char *name;
int type;
struct ltable *next;
}*lhead;
struct ltable* llookup(char *name);
void linstall(char *name,int type);


%}

%union{
struct node{
char* name;
int type;
struct node *c1,*c2,*c3;
int value;
}*ptr;
}

%token <ptr> LOGOP ID DO MAIN THEN BEG READ WRITE ELSE BOOLEAN INTEGER ENDDECL NUMBER IF DECL END WHILE ENDWHILE ENDIF RETURN SEMCO LSB BIN
%token <ptr> RSB COMMA RBR LBR LCB RCB ASSI
%right <ptr> NOT
%left <ptr> AND OR
%left <ptr> RELOP
%left <ptr> PLUS MINUS
%left <ptr> MUL DIV REM
%type <ptr> global main gdeclares gdeclare gtype gvariables gvariable ldeclares ldeclare ltype lvariables lvariable mbody statements statement assignment conditional iterative io expression returnstat

%%
start:	global main	{return 0;}
	| main		{return 0;}
	;
	
global:	DECL gdeclares ENDDECL	{}
	;

gdeclares:	gdeclares gdeclare	{}
		|gdeclare	{}
		;
			
gdeclare:	gtype gvariables SEMCO	{}
		;

gtype:	INTEGER {vtype=0;}
	|BOOLEAN {vtype=1;}
	;
	
gvariables:	gvariables COMMA gvariable	{}	
		|gvariable	{}
		;
		
gvariable:	ID	{ginstall($1->name,vtype,1);}	
		|ID LSB NUMBER RSB	{ginstall($1->name,vtype,$3->value);}
		;
		
main:	INTEGER MAIN LBR RBR LCB mbody RCB	{}
	;
	
mbody:	local BEG statements returnstat END		{}
	;
	
local: DECL ldeclares ENDDECL {}
	| {}
	;
	
ldeclares:	ldeclares ldeclare	{}
		|ldeclare	{}
		;
			
ldeclare:	ltype lvariables SEMCO	{}
		;

ltype:	INTEGER {vtype=0;}
	|BOOLEAN {vtype=1;}
	;
	
lvariables:	lvariables COMMA lvariable	{}	
		|lvariable	{}
		;	
		
lvariable:	ID	{linstall($1->name,vtype);}	
		;
	
statements:	statements statement {}
		|		{}
		;
		
statement:	assignment SEMCO	{}
		|conditional SEMCO	{}
		|iterative SEMCO	{}	
		|io SEMCO	{}
		;	
		
conditional: 	IF LBR expression RBR THEN statements ELSE statements ENDIF{}
		|IF LBR expression RBR THEN statements	ENDIF	{}
		;
		
iterative:	WHILE LBR expression RBR DO statements ENDWHILE	{}
		;
		
io:	READ LBR ID RBR	{	
				struct ltable *ltemp=llookup($3->name);
				if(ltemp==NULL)
				{
					struct gtable *gtemp=glookup($3->name);
					if(gtemp==NULL)
					printf("No such variable to read");
				}
			
			}
	|WRITE LBR expression RBR {}
	;
	
assignment:	ID ASSI expression	{	struct ltable *ltemp=llookup($1->name);
						if(ltemp==NULL)
						{
							struct gtable *gtemp=glookup($1->name);
							if(gtemp==NULL)
								yyerror("No such variable to assign");
							else
								$$->type=gtemp->type;
						}
						else
							$$->type=ltemp->type;
						if($1->type!=$3->type)
						yyerror("Type Mismatch in Assignment");
					}
		;
		
		
expression:	 expression PLUS expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression MINUS expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression MUL expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression DIV expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression REM expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|LBR expression RBR				{$$=$2;}
		|expression RELOP expression	{
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression AND expression	{
							if($1->type!=$3->type&&$1->type!=1)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression OR expression	{
							if($1->type!=$3->type&&$1->type!=1)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=1;
						}
		|NOT expression			{
							if($2->type!=1)
							printf("Type Mismatch in Assignment");
							else
							$$->type=1;
						}
		|ID				{
						struct ltable *ltemp=llookup($1->name);
						if(ltemp==NULL)
						{
							struct gtable *gtemp=glookup($1->name);
							if(gtemp==NULL)
								yyerror("No such variable to read");
							else
								$$->type=gtemp->type;
						}
						else
							$$->type=ltemp->type;
						}
		|ID LSB expression RSB		{if($3->type!=0)
							yyerror("Array bound error");
						else
						{
						
							struct gtable *gtemp=glookup($1->name);
							if(gtemp==NULL)
								yyerror("No such integer array");
							else
							{
								if(gtemp->type==1)
									yyerror("No such integer array");
								else
								$$->type=gtemp->type;
							}
						}
						}
						
						
		|NUMBER				{$$->type=0;}
		|BIN				{$$->type=1;}
		;

returnstat: RETURN expression SEMCO	{}
		;
		
%%

int main()
{
yyparse();
printf("\n\n Parsed Without Any Error  ---  Congratulations \n\n\n");
return 0;
}

void ginstall(char *name,int type,int size)
{
struct gtable *curr=malloc(sizeof(struct gtable));
curr->name=name;
curr->type=type;
curr->size=size;
curr->next=ghead;
ghead=curr;
}

struct gtable* glookup(char *name)
{
struct gtable *temp=malloc(sizeof(struct gtable));
temp=ghead;
while(temp!=NULL)
{
if(strcmp(temp->name,name)==0)
return temp;
temp=temp->next;
}
return NULL;
}
void linstall(char *name,int type)
{
struct ltable *curr=malloc(sizeof(struct ltable));
curr->name=name;
curr->type=type;
curr->next=lhead;
lhead=curr;
}

struct ltable* llookup(char *name)
{
struct ltable *temp=malloc(sizeof(struct ltable));
temp=lhead;
while(temp!=NULL)
{
if(strcmp(temp->name,name)==0)
return temp;
temp=temp->next;
}
return NULL;
}
