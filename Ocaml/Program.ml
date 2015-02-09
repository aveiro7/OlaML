open Parser
open Lexer

let x = "2 + 2"


let lexbuf = Lexing x

let y = Parser.program Lexer.tokenize lexbuf

print y