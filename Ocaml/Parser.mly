{%
	open GrammarRules
%}

%token <int> INT
%token PLUS

%start program
%type <GrammarRules.expr list> program

%%

program:
	| expr					{ [$1] }
	| expr program	{ $1 :: $2 }

expr:
	| INT PLUS INT 	{ $1 + $2 }

%%