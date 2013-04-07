%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
void yyerror(char *s);
struct node* makenodey(char nodetype,char* name,int value);
struct node* maketree( struct node* noder, struct node* c1, struct node* c2, struct node* c3);
void printtree(struct node * ptr);
int echk=0,tempc=1,ifc=1,wc=1,f=0,regc=0,idc=0;
int vtype;
struct node* treehead;
struct gtable
{
char *name;
int type;
int size;
int bind;
struct gtable *next;
}*ghead;
struct gtable* glookup(char *name);
void ginstall(char *name,int type,int size);

struct ltable
{
char *name;
int type;
int bind;
struct ltable *next;
}*lhead;
struct ltable* llookup(char *name);
void linstall(char *name,int type);

struct tac
{
char op;
char s1[25],s2[25],dest[25];
struct tac * next;
}*tacf,*tacr;

char * gentac(struct node * treehead);
void printtac(struct tac * tacf);
void gencode(struct tac * tacf);
int genvct(char *name);
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
		
gvariable:	ID	{
			struct gtable *gtemp=glookup($1->name);
			if(gtemp==NULL)
			ginstall($1->name,vtype,1);
			else
			yyerror("Variable Already Exists");
			}	
		|ID LSB NUMBER RSB	{struct gtable *gtemp=glookup($1->name);
					if(gtemp==NULL)
					ginstall($1->name,vtype,$3->value);
					else
					yyerror("Variable already exists");}
		;
		
main:	INTEGER MAIN LBR RBR LCB mbody RCB	{}
	;
	
mbody:	local BEG statements returnstat END		{$$=makenodey('m',NULL,0);
							 $$=maketree($$,$3,$4,NULL);
								}
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
		
lvariable:	ID	{struct ltable *ltemp=llookup($1->name);
			if(ltemp==NULL)
			linstall($1->name,vtype);
			else
			yyerror("Variable Already Exists");
			}	
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
						yyerror("Type Mismatch in Assignment1");
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
							$$->type=1;
						}
		|expression AND expression	{	
							$$=maketree($2,$1,$3,NULL);
							if($1->type!=$3->type&&$1->type!=1)
							yyerror("Type Mismatch in Assignment");
							
							else
							$$->type=1;
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

returnstat: RETURN expression SEMCO	{$$=makenodey('x',NULL,0);
					 $$=maketree($$,$2,NULL,NULL);}
		;
		
%%

int main()
{
FILE *fp;
fp = fopen("sim.asm","w");
fprintf(fp,"START\n");
fprintf(fp,"MOV SP, 0\n");
fprintf(fp,"MOV BP, 0\n");
fclose(fp);
yyparse();
if(echk==0)
{
printf("\n\n Compiled Without Any Error  ---  Congratulations \n\n\n");
printtree(treehead);
char tem[25];
strcpy(tem,gentac(treehead));
printtac(tacf);
gencode(tacf);
fp=fopen("sim.asm","a");
fprintf(fp,"HALT\n");
}
else
printf("\n\n Please correct the specified errors and compile again :( \n\n\n");
fclose(fp);
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
curr->bind=idc;
idc++;
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
curr->bind=idc;
idc++;
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

char * gentac(struct node *treehead)
{
if(treehead==NULL)
{
char tem[25];
strcpy(tem,"");
return tem;
}
if(treehead->nodetype=='+'||treehead->nodetype=='-'||treehead->nodetype=='*'||treehead->nodetype=='/'||treehead->nodetype=='%'||treehead->nodetype=='o'||treehead->nodetype=='a'||treehead->nodetype=='g'||treehead->nodetype=='l'||treehead->nodetype=='G'||treehead->nodetype=='L'||treehead->nodetype=='e'||treehead->nodetype=='N')
{
struct tac * new=malloc(sizeof(struct tac));
new->op=treehead->nodetype;
strcpy(new->s1,gentac(treehead->c1));
strcpy(new->s2,gentac(treehead->c2));
sprintf(new->dest,"_temp%d",tempc);
tempc++;
if(f==0)
{
f=1;
tacf=tacr=new;
}
else
{
tacr->next=new;
tacr=new;
}
return new->dest;
}
if(treehead->nodetype=='s')
{
struct tac * new=malloc(sizeof(struct tac));
new->op=treehead->nodetype;
strcpy(new->s1,gentac(treehead->c1));
strcpy(new->s2,gentac(treehead->c2));
strcpy(new->dest,"");
return new->dest;
}
if(treehead->nodetype=='=')
{
struct tac * new=malloc(sizeof(struct tac));
new->op=treehead->nodetype;
strcpy(new->s1,gentac(treehead->c2));
strcpy(new->s2,gentac(treehead->c3));
strcpy(new->dest,gentac(treehead->c1));
if(f==0)
{
f=1;
tacf=tacr=new;
}
else
{
tacr->next=new;
tacr=new;
}
return new->dest;
}
if(treehead->nodetype=='v')
{
return treehead->name;
}
if(treehead->nodetype=='c'||treehead->nodetype=='b')
{
char tem[25];
sprintf(tem,"%d",treehead->value);
return tem;
}
if(treehead->nodetype=='m')
{	
char tem[25];
strcpy(tem,gentac(treehead->c1));
strcpy(tem,gentac(treehead->c2));
strcpy(tem,"MAIN");
return tem;
}
if(treehead->nodetype=='I')
	{
	int k;
	struct tac * new=malloc(sizeof(struct tac));
	new->op=treehead->nodetype;
	strcpy(new->s1,gentac(treehead->c1));
	strcpy(new->s2,"goto");
	sprintf(new->dest,"L%d",ifc);
	tacr->next=new;
	tacr=new;
	char tem[25];
	struct tac * new1=malloc(sizeof(struct tac));
	new1->op=treehead->nodetype;
	new1->op='Z';
	sprintf(new1->s1,"L%d:",ifc);
	ifc++;
	struct tac * new3=malloc(sizeof(struct tac));
	new3->op=treehead->nodetype;
	new3->op='J';
	sprintf(new3->s1,"L%d",ifc);
	struct tac * new2=malloc(sizeof(struct tac));
	new2->op=treehead->nodetype;
	new2->op='Z';
	sprintf(new2->s1,"L%d:",ifc);
	ifc++;
	if(treehead->c3!=NULL)
		{
		strcpy(tem,gentac(treehead->c3));
		}
	tacr->next=new3;
	tacr=new3;
	tacr->next=new1;
	tacr=new1;
	char tem1[25];
	strcpy(tem1,gentac(treehead->c2));
	tacr->next=new2;
	tacr=new2;
	char temr[25];
	strcpy(temr,"IF");
	return temr;
	}
	
if(treehead->nodetype=='W')
	{
	struct tac * new=malloc(sizeof(struct tac));
	new->op='Y';
	struct tac * new5=malloc(sizeof(struct tac));
	new5->op='J';
	sprintf(new5->s1,"W%d",wc);
	sprintf(new->s1,"W%d:",wc);
	wc++;
	tacr->next=new;
	tacr=new;
	struct tac * new1=malloc(sizeof(struct tac));
	new1->op='I';
	strcpy(new1->s1,gentac(treehead->c1));
	strcpy(new1->s2,"goto");
	sprintf(new1->dest,"L%d",ifc);
	tacr->next=new1;
	tacr=new1;
	struct tac * new2=malloc(sizeof(struct tac));
	new2->op='Z';
	sprintf(new2->s1,"L%d:",ifc);
	ifc++;
	struct tac * new3=malloc(sizeof(struct tac));
	new3->op='J';
	sprintf(new3->s1,"W%d",wc);
	struct tac * new4=malloc(sizeof(struct tac));
	new4->op='Y';
	sprintf(new4->s1,"W%d:",wc);
	wc++;
	tacr->next=new3;
	tacr=new3;
	tacr->next=new2;
	tacr=new2;
	char tem1[25];
	strcpy(tem1,gentac(treehead->c2));
	tacr->next=new5;
	tacr=new5;
	tacr->next=new4;
	tacr=new4;
	char temr[25];
	strcpy(temr,"WHILE");
	return temr;
	}
	
if(treehead->nodetype=='r'||treehead->nodetype=='w'||treehead->nodetype=='n'||treehead->nodetype=='x')
	{
	struct tac * new=malloc(sizeof(struct tac));
	new->op=treehead->nodetype;
	strcpy(new->s1,gentac(treehead->c1));
	if(treehead->nodetype=='n')
		{
		sprintf(new->dest,"_temp%d",tempc);
		tempc++;
		tacr->next=new;
		tacr=new;
		return new->dest;
		}
	tacr->next=new;
	tacr=new;
	char tem[25];
	strcpy(tem,"");
	return tem;
	}	
	
	
		
	
char tem[25];
strcpy(tem,"");
return tem;
}

void printtac(struct tac * tacf)
{
if(tacf==NULL)
printf("Its Over");
else
{
printf("\n%c\t%s\t%s\t%s\n",tacf->op,tacf->s1,tacf->s2,tacf->dest);
printtac(tacf->next);
}
}

void gencode(struct tac * tacf)
{	
if(tacf->op=='=')
	{
	FILE *fp;
	fp = fopen("sim.asm","a");
	struct ltable * new=malloc(sizeof(struct ltable));
	new=llookup(tacf->dest);
	if(new==NULL)
		new=glookup(tacf->dest);
	regc++;
	fprintf(fp,"MOV R%d,BP\n",regc);				//1
	regc++;
	fprintf(fp,"MOV R%d,%d\n",regc, new->bind);		//2	
	regc++;
	fprintf(fp,"ADD R%d,R%d\n",regc-2, regc-1);							//1 0   
	fclose(fp);
	fp = fopen("sim.asm","a");
	if(tacf->s1[0]=='_')
		{
		fprintf(fp,"MOV R%d,R%d\n",regc,regc-3);		//2 0
		fprintf(fp,"MOV [R%d],R%d\n",regc-2,regc);		//2 1
		regc=regc-3;
		}
	if(isdigit(tacf->s1[0]))
		{
		fprintf(fp,"MOV R%d,%d\n",regc-1,atoi(tacf->s1));
		fprintf(fp,"MOV [R%d],R%d\n",regc-2,regc-1);
		regc=regc-3;
		}
	if(isalpha(tacf->s1[0]))
		{
		regc++;
		int r=genvar(tacf->s1);
		fprintf(fp,"MOV [R%d],R%d\n",regc-3,r);
		regc=regc-4;
		}
	fclose(fp);	
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='+')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"ADD R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc=regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='-')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"SUB R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='*')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"MUL R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='/')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"DIV R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='%')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"MOD R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='a')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"MUL R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='o')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"ADD R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='g')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"GT R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='l')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"LT R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='G')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"GE R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='L')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"LE R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='N')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"NE R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='e')
	{
	int r=genvct(tacf->dest);
	int r1=genvct(tacf->s1);
	int r2=genvct(tacf->s2);
	
	FILE *fp;
	fp = fopen("sim.asm","a");
	fprintf(fp,"EQ R%d,R%d\n",r1,r2);
	fprintf(fp,"MOV R%d,R%d\n",r,r1);
	regc=regc-2;
	fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
	regc--;
	fclose(fp);
	gencode(tacf->next);
	return;
	}
	
if(tacf->op=='n')
{
regc++;
FILE *fp;
fp = fopen("sim.asm","a");	
fprintf(fp,"MOV R%d,1\n",regc);
int r=genvct(tacf->s1);
fprintf(fp,"SUB R%d,R%d\n",regc-1,r);
regc--;	
fprintf(fp,"MOV R%d,R%d\n",regc-1,regc);
regc--;
fclose(fp);
gencode(tacf->next);
return;
}
	
if(tacf->op=='r')
{
FILE *fp;
regc++;
fp = fopen("sim.asm","a");
fprintf(fp,"IN R%d\n",regc);
int r=genvct(tacf->s1);
regc--;
fprintf(fp,"MOV [R%d],R%d\n",r+1,regc);
fclose(fp);
regc--;
gencode(tacf->next);
return;
}

if(tacf->op=='w')
{
FILE *fp;
fp = fopen("sim.asm","a");
int r=genvct(tacf->s1);
regc--;
fprintf(fp,"OUT R%d\n",r);
fclose(fp);
regc--;
gencode(tacf->next);
return;
}

if(tacf->op=='Y')
{
FILE *fp;
fp = fopen("sim.asm","a");
fprintf(fp,"%s\n",tacf->s1);
fclose(fp);
gencode(tacf->next);
return;
}

if(tacf->op=='Z')
{
FILE *fp;
fp = fopen("sim.asm","a");
fprintf(fp,"%s\n",tacf->s1);
fclose(fp);
gencode(tacf->next);
return;
}	
	
if(tacf->op=='I')
{
FILE *fp;
fp = fopen("sim.asm","a");
fprintf(fp,"MOV R%d,1\n",regc+1);
fprintf(fp,"SUB R%d,R%d\n",regc+1,regc);
fprintf(fp,"JZ R%d %s\n",regc+1,tacf->dest);
fclose(fp);
gencode(tacf->next);
return;
}	

if(tacf->op=='J')
{
FILE *fp;
fp = fopen("sim.asm","a");
fprintf(fp,"JMP %s\n",tacf->s1);
fclose(fp);
gencode(tacf->next);
return;
}

	
	
	
	
		
else
return;
}

int genvct(char *name)
{
FILE *fp;
fp = fopen("sim.asm","a");
if(name[0]=='_')
	{
	regc++;
	fprintf(fp,"MOV R%d,R%d\n",regc,regc-1);
	fclose(fp);
	return regc;
	}
if(isalpha(name[0]))
	{
	regc++;
	fclose(fp);
	return genvar(name);
	}
if(isdigit(name[0]))
	{
	regc++;
	fprintf(fp,"MOV R%d,%d\n",regc,atoi(name));
	fclose(fp);
	return regc;
	}	
}	
int genvar(char * name)
{
FILE *fp;
fp = fopen("sim.asm","a");
struct ltable * new=malloc(sizeof(struct ltable));
new=llookup(name);
if(new==NULL)
	new=glookup(name);
fprintf(fp,"MOV R%d,BP\n",regc);				//0
regc++;
fprintf(fp,"MOV R%d,%d\n",regc, new->bind);		//1	
regc++;
fprintf(fp,"ADD R%d,R%d\n",regc-1, regc-2);			//1 0   
fprintf(fp,"MOV R%d,[R%d]\n", regc-2, regc-1);			//0 1
regc=regc-2;
fclose(fp);	
return 	regc;
}
void yyerror(char *s)
{
printf("\n\n%s\n\n",s);
echk=1;
}
