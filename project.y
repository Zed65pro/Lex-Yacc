%{
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void yyerror(char *s);
int yylex();
int yydebug = 1;

#define res 36 // number of reserved words
#define size 52
// list of reserved variables
char reserved[res][size]={"if","while","else","for","long","typedef","switch","int",
"float","double","void","char","sizeof","const","break","continue","struct","do","union","enum","extern"
,"default","case","signed","unsigned","volatile","extern","aslong","is","elis","or","event","repeat","until"
, "is" , "oris"
};

typedef struct value{ // struct for types that can be in an identifier

	int d;
	double f;
	char c;
	char string[size];

}value;

typedef struct node{ // struct for an identifier

	char str[size]; // name of identifier
	value val; //
	int flag;// flag=0 uninitilized,flag=1 int,flag=2 double,flag=3 char,flag=4 string
	struct node* next;

}node;

node* id_list=NULL; // identifier list
value val;

//Functions used to store values in identifiers and save them.
int update_id(int flag,value data,char* str); // updates identifier with given value
int check_res(char* str); // checks if identifier resembles a reserved word
int get_val_i(char* str); // gets identifier int value
char get_val_c(char* str); // gets identifer char value
double get_val_f(char* str); // gets identifier float value;
char* get_val_s(char* str); // gets identifier char* value(string)

void help_fun();
%}

%union {
  int d;
  char* id;
  double f;
  char c;
  char str[30];
};

%start program
%token exit_command start_command PRINT tok_help
%token <d> number
%token <f> FLOAT
%token <c> character;

%token great_eq less_eq eq not_eq UMinus
%token TAN COS SIN LOG
%token IS ORIS OR DO END ASLONG EVENTS EVENT REPEAT UNTIL END_EVENTS
%token tok_int tok_float tok_char tok_string

%token <id> identifier
%type <d> bool_exp arit_exp term factor
%type <f> arit_exp_f term_f factor_f

%type <c> exp_char
%token <str> STR
%type <str> exp_string bool_exp_str
%type <id> assignment

%right '^'
%right great_eq less_eq
%right not_eq eq
%left '+' '-'
%left '*' '/'
%left UMinus

%%
program: start_command start
       ;
start:
    | start line
    ;

line: bool_exp   ';'   { ; }
    | assignment   ';'  {;}
    | exit_command      { printf("OK\n"); exit(EXIT_SUCCESS); }
    | PRINT '(' bool_exp ')' ';'      { printf("%d\n", $3); }
    | PRINT '(' arit_exp_f ')'';'      { printf("%.2f\n", $3); }
    | PRINT '(' exp_char ')' ';'     { printf("%c\n", $3); }
    | PRINT '(' bool_exp_str ')'';'      { printf("%s\n", $3); }
		| condition_stmnt
		| loop_stmnt
		| help
    ;

help: tok_help {help_fun();}
		;

condition_stmnt:  IS '(' bool_exp ')' '?' DO start END
				  | IS '(' bool_exp ')' '?' DO start END OR start END
					| IS '(' bool_exp ')' '?' DO start END oris_stmnt
					| IS '(' bool_exp ')' '?' DO start END oris_stmnt OR DO start END
					| events_stmnt
					;

oris_stmnt: ORIS '(' bool_exp ')' '?' DO start END
					| oris_stmnt ORIS '(' bool_exp ')' '?' DO start END
					;
events_stmnt: EVENTS identifier '.' tok_int   ':' event_stmnt_int END_EVENTS
						| EVENTS identifier '.' tok_float   ':' event_stmnt_float END_EVENTS
						| EVENTS identifier '.' tok_char   ':' event_stmnt_char END_EVENTS
						| EVENTS identifier '.' tok_string   ':' event_stmnt_string END_EVENTS
						;

event_stmnt_int: EVENT number ':' start END
						| event_stmnt_int EVENT number ':' start END
					  ;

event_stmnt_float: EVENT FLOAT ':' start END
						| event_stmnt_float EVENT FLOAT ':' start END
						;

event_stmnt_char: EVENT character ':' start END
							| event_stmnt_char EVENT character ':' start END
							;

event_stmnt_string: EVENT STR ':' start END
							| event_stmnt_string EVENT STR ':' start END
							;

loop_stmnt: ASLONG '(' bool_exp ')' DO start END
					| REPEAT start  UNTIL'(' bool_exp ')' ';'
					;

bool_exp:   arit_exp            { $$=$1; }
        | bool_exp '<' arit_exp     { $$ = $1 < $3; }
        | bool_exp '>' arit_exp     { $$ = $1 > $3; }
				| bool_exp great_eq arit_exp {$$= $1 >= $3;}
				| bool_exp less_eq arit_exp {$$= $1 <= $3;}
				| bool_exp eq arit_exp {$$= $1 == $3;}
				| bool_exp not_eq arit_exp {$$= $1 != $3;}

        | '+' '+' identifier
        {
          val.d=get_val_i($3)+1;
          update_id(1, val,$3); $$ = get_val_i($3); }
        | '-' '-' identifier
        {
          val.d=get_val_i($3)-1;
          update_id(1, val ,$3); $$ = get_val_i($3);
					}
        ;

arit_exp:   term            { $$ = $1; }
        | arit_exp '+' term { $$ = $1 + $3; }
        | arit_exp '-' term { $$ = $1 - $3; }
				| '-' term {$$= -$2;}
        ;

term:   factor          { $$ = $1;  }
        | term '*' factor   { $$ = $1 * $3;  }
        | term '/' factor   { $$ = $1 / $3;  }
        | term '%' factor   { $$ = $1 % $3; }
    		| term '^' factor   { $$ = pow($1, $3); }
    		;

factor: number          { $$ = $1; }
    		| identifier '.' tok_int       { $$ = get_val_i($1); }
        | '(' bool_exp ')'  { $$ = $2; }
    		;

arit_exp_f:   term_f            { $$ = $1; }
            | arit_exp_f '+' term_f { $$ = $1 + $3; }
            | arit_exp_f '-' term_f { $$ = $1 - $3; }
						| UMinus term_f {$$= -$2;}
            ;

term_f:   factor_f          { $$ = $1;  }
        | term_f '*' factor_f   { $$ = $1 * $3;  }
        | term_f '/' factor_f   { $$ = $1 / $3;  }
        | term_f '^' factor_f   { $$ = pow($1, $3); }
        | term_f '^' factor   { $$ = pow($1, $3); }
        ;

factor_f:   FLOAT          { $$ = $1; }
	        | identifier '.' tok_float { $$ = get_val_f($1); }
	        | '(' arit_exp_f ')'  { $$ = $2; }
					| TAN '(' arit_exp_f ')' {$$=tan($3);}
					| COS '(' arit_exp_f ')' {$$=cos($3);}
					| SIN '(' arit_exp_f ')' {$$=sin($3);}
					| LOG '(' arit_exp_f ')' {$$=log($3);}
					| TAN '(' bool_exp ')' {$$=tan($3);}
					| COS '(' bool_exp ')' {$$=cos($3);}
					| SIN '(' bool_exp ')' {$$=sin($3);}
					| LOG '(' bool_exp ')' {$$=log($3);}
	        ;

exp_char: identifier '.' tok_char {$$=get_val_c($1);}
  			| character {$$=$1;}
  			;

bool_exp_str: exp_string  {strcpy($$,$1);}
					| bool_exp_str '>' exp_string
					{
						if(strcmp($1,$3)>0)
							strcpy($$,"1");
						else
							strcpy($$,"0");
					}
					| bool_exp_str '<' exp_string
					{
						if(strcmp($1,$3)<0)
							strcpy($$,"1");
						else
							strcpy($$,"0");
					}
					| bool_exp_str eq exp_string
					{
						if(strcmp($1,$3)==0)
							strcpy($$,"1");
						else
							strcpy($$,"0");
					}
					| bool_exp_str not_eq exp_string
					{
						if(strcmp($1,$3)!=0)
							strcpy($$,"1");
						else
							strcpy($$,"0");
					}
					| bool_exp_str '+' exp_string { strcpy($$,$1); strcat($$,$3);}
					;
exp_string: identifier '.' tok_string {strcpy($$,get_val_s($1));}
  	  | STR {strcpy($$,$1);}
  			;

assignment: identifier '=' bool_exp
	{
      val.d=$3;

      if(update_id(1,val, $1))
        //printf("> %s <- %d\n> ", $1, $3);
				;
      else{
				yyerror("Syntax error,variable already defined with another type!");
				exit(1);
			}
  }
  | identifier '=' arit_exp_f
	{
      val.f=$3;
      if(update_id(2,val, $1))
        //printf("> %s <- %.2f\n> ", $1, $3);
				;
      else{
				yyerror("Syntax error,variable already defined with another type!");
				exit(1);
			}
  }
  | identifier '=' character
	{
      val.c=$3;
      if(update_id(3,val, $1))
        //printf("> %s <- %c\n> ", $1, $3);
				;
      else{
				yyerror("Syntax error,variable already defined with another type!");
				exit(1);
			}
  }
  | identifier '=' bool_exp_str
	{
      strcpy(val.string,$3);
      if(update_id(4,val, $1))
        //printf("> %s <- %s\n> ", $1, $3);
				;
      else{
				yyerror("Syntax error,variable already defined with another type!");
				exit(1);
			}
      }
  ;

%%


int update_id(int flag,value data,char str[]){

	check_res(str);

	if(id_list==NULL){
		id_list=(node*)calloc(1,sizeof(node));
		strcpy(id_list->str,str);

		if(flag==1){
			id_list->flag=1;
			id_list->val.d=data.d;
		}
		else if(flag==2){
			id_list->flag=2;
			id_list->val.f=data.f;
		}
		else if(flag==3){
			id_list->flag=3;
			id_list->val.c=data.c;
		}
		else if(flag==4){
			id_list->flag=4;
			strcpy(id_list->val.string,data.string);
		}

    return 1;
	}
	else{
		node* ptr=id_list;
		node* prev=NULL;

		while(ptr){

			if(strcmp(str,ptr->str)==0)
				break;

			prev=ptr;
			ptr=ptr->next;

		}

		if(ptr){
			if(ptr->flag==1 && ptr->flag==flag){
        ptr->val.d=data.d;
        return 1;
      }

			else if(ptr->flag==2 && ptr->flag==flag){
        ptr->val.f=data.f;
        return 1;
      }

			else if(ptr->flag==3 && ptr->flag==flag){
        ptr->val.c=data.c;
        return 1;
      }

			else if(ptr->flag==4 && ptr->flag==flag){
        strcpy(ptr->val.string,data.string);
        return 1;
      }

		}
		else{
			node* temp=(node*)calloc(1,sizeof(node));
			strcpy(temp->str,str);

			if(flag==1){
				temp->flag=1;
				temp->val.d=data.d;
			}
			else if(flag==2){
				temp->flag=2;
				temp->val.f=data.f;
			}
			else if(flag==3){
				temp->flag=3;
				temp->val.c=data.c;
			}
			else if(flag==4){
				temp->flag=4;
				strcpy(temp->val.string,data.string);
			}

			prev->next=temp;

      return 1;
		}

	}
  return 0;
}


int get_val_i(char* str){

	if(id_list){

		node* ptr=id_list;

		while(ptr){
			if(strcmp(ptr->str,str)==0){
				if(ptr->flag==1)
					return ptr->val.d;
				else{
					yyerror("syntax error, Expected type of variable is not int.");
					exit(1);
				}

			}
			ptr=ptr->next;
		}
		if(ptr==NULL){
			yyerror("syntax error, Undefined variable.");
			exit(1);
		}

	}
	else{
		yyerror("syntax error, Undefined variable.");
		exit(1);
	}

}

char get_val_c(char* str){

	if(id_list){

		node* ptr=id_list;

		while(ptr){
			if(strcmp(ptr->str,str)==0){
				if(ptr->flag==3)
					return ptr->val.c;
				else{
					yyerror("syntax error, Expected type of variable is not char.");
					exit(1);
				}
			}
			ptr=ptr->next;
		}
		if(ptr==NULL){
			yyerror("syntax error, Undefined variable.");
			exit(1);
		}
	}
	else{
		yyerror("syntax error, Undefined variable.");
		exit(1);
	}

}

double get_val_f(char* str){

	if(id_list){

		node* ptr=id_list;

		while(ptr){
			if(strcmp(ptr->str,str)==0){
				if(ptr->flag==2)
					return ptr->val.f;
				else{
					yyerror("syntax error, Expected type of variable is not float.");
					exit(1);
				}
			}
			ptr=ptr->next;
		}
		if(ptr==NULL){
			yyerror("syntax error, Undefined variable.");
			exit(1);
		}
	}
	else{
		yyerror("syntax error, Undefined variable.");
		exit(1);
	}
}


char* get_val_s(char* str){

	if(id_list){

		node* ptr=id_list;

		while(ptr){
			if(strcmp(ptr->str,str)==0){
				if(ptr->flag==4)
					return ptr->val.string;
				else{
					yyerror("syntax error, Expected type of variable is not string.");
					exit(1);
				}
			}
				ptr=ptr->next;
			}
			if(ptr==NULL){
				yyerror("syntax error, Undefined variable.");
				exit(1);
			}
	}
	else{
		yyerror("syntax error, Undefined variable.");
		exit(1);
	}
}

int check_res(char* str){

	for(int i=0;i<res;++i)
		if(strcmp(str,reserved[i])==0){
			yyerror("syntax error, Reserved word used!");
			exit(1);
		}

	if(!isalpha(str[0])){
		yyerror("syntax error,Variables names can only begin with letters.");
		exit(1);
	}

	for(int i=0;str[i]!='\0';++i){
		if(isalpha(str[i]) || isdigit(str[i]) || str[i]=='_')
			continue;
	}

}

void help_fun(){

		printf("This is a basic language which can do basic arithmetic operations on various data types,\n");
		printf("includes two types of conditional statements: is()? and events: \n");
		printf("Also, includes two types of iterative statements: aslong() and repeat-until,\n");
		printf("Also, can output various expressions according to their data types.\n");
		//printf("Press '1' for more info about how identifiers and assignment work. \n")
		//printf("Press '2' for more info about arithmetic and basic operations. \n")
		//printf("Press '3' for more info about conditional statements.\n")
		//printf("Press '4' for more info about iterative statements.\n")
		//printf("Press '5' for more info about printing results.\n")

}

int main(void) {

		printf("--------------------------------------------------------------\n");
    printf("Welcome, ");
		printf("To start the program type \"main=1\" and to end \"main=0\". \n");
		printf("For more information type \"-help\" in the command interface.\n");
		printf("------------------------------------------------------------\n");
    return yyparse();
}

void yyerror(char *s) {
	  printf("\n---------------------------------------\n");
		fprintf(stderr, "Error: %s\n", s);
		printf("---------------------------------------\n");
	}
