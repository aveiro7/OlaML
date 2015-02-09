{
	open GrammarRules
	exception LexingError of string
}

let integer = ['0'-'9']+


rule tokenize = parse
	| [' ' '\t' '\r' '\n']		{ tokenize lexbuf }
	| integer									{ INT (Lexeme.lexbuf) }
	| '+'											{ PLUS }
	| _												{ raise LexingError "lexing error - unknown char"}
