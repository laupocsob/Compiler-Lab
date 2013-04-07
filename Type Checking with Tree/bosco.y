%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
void yyerror(char *s);
struct node* makenodey(char nodetype,char* name,int value);
struct node* maketree( struct node* noder, struct node* c1, struct node* c2, struct node* c3);
void printtree(struct node * ptr);
int echk=0;
int vtype;
struct node* treehead;
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
char nodetype;
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
	
statements:			{$$=NULL;}
		|statements statement {
					$$=makenodey('s',NULL,0);
					$$=maketree($$,$1,$2,NULL);
				      }
		
		;
		
statement:	assignment SEMCO	{$$=$1;}
		|conditional SEMCO	{$$=$1;}
		|iterative SEMCO	{$$=$1;}	
		|io SEMCO	{$$=$1;}
		;	
		
conditional: 	IF LBR expression RBR THEN statements ELSE statements ENDIF{$$=maketree($1,$3,$6,$8);}
		|IF LBR expression RBR THEN statements	ENDIF	{$$=maketree($1,$3,$6,NULL);}
		;
		
iterative:	WHILE LBR expression RBR DO statements ENDWHILE	{$$=maketree($1,$3,$6,NULL);}
		;
		
io:	READ LBR ID RBR	{	$$=maketree($1,$3,NULL,NULL);
				struct ltable *ltemp=llookup($3->name);
				if(ltemp==NULL)
				{
					struct gtable *gtemp=glookup($3->name);
					if(gtemp==NULL)
					printf("No such variable to read");
				}
			
			}
	|WRITE LBR expression RBR {$$=maketree($1,$3,NULL,NULL);}
	;
	
assignment:	ID ASSI expression	{	
						$$=maketree($2,$1,$3,NULL);
						struct ltable *ltemp=llookup($1->name);
						if(ltemp==NULL)
						{
							struct gtable *gtemp=glookup($1->name);
							if(gtemp==NULL)
								yyerror("No such variable to assign");
							else
								$1->type=gtemp->type;
						}
						else
							$1->type=ltemp->type;
						if($1->type!=$3->type)
						yyerror("Type Mismatch in Assignment");
					}
		;
		
		
expression:	 expression PLUS expression	{
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression MINUS expression	{
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression MUL expression	{
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression DIV expression	{	
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression REM expression	{	
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|LBR expression RBR				{$$=$2;}
		|expression RELOP expression	{	
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=0)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression AND expression	{	
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=1)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=0;
						}
		|expression OR expression	{
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=1)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=1;
						}
		|NOT expression			{
							$$=maketree($1,$2,NULL,NULL);
							if($2->type!=1)
							printf("Type Mismatch in Assignment");
							else
							$$->type=1;
						}
		|ID				{
						$$=$1;
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
								{
								$$=maketree($1,$3,NULL,NULL);
								$$->type=gtemp->type;
								}
							}
						}
						}
						
						
		|NUMBER				{$$=$1;$$->type=0;}
		|BIN				{$$=$1;$$->type=1;}
		;

returnstat: RETURN expression SEMCO	{}
		;
		
%%

int main()
{
yyparse();
if(echk==0)
{
printf("\n\n Compiled Without Any Error  ---  Congratulations \n\n\n");
printtree(treehead);
}
else
printf("\n\n Please correct the specified errors and compile again :( \n\n\n");
return 0;
}

struct node* makenodey(char nodetype,char* name,int value)
{
struct node* temp=malloc(sizeof(struct node));
temp->name=name;
temp->nodetype=nodetype;
temp->c1=NULL;
temp->c2=NULL;
temp->c3=NULL;
temp->value=value;
temp->type=-1;
return temp;
}

struct node* maketree( struct node* noder, struct node* c1, struct node* c2, struct node* c3)
{ 
 	struct node* temp=noder;
	temp->c1=c1;
	temp->c2=c2;
	temp->c3=c3;
	treehead=temp;
	return temp;
}

void printtree(struct node * ptr)
{

	if(ptr==NULL)
		return;
	else
	{
	printf(" ( ");
	printf("%c", ptr->nodetype);
	printtree(ptr->c1);
	printtree(ptr->c2);
	printtree(ptr->c3);
	printf(" ) ");
	}
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

void yyerror(char *s)
{
printf("\n\n%s\n\n",s);
echk=1;
}
