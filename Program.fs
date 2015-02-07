module Program

open System
open GrammarRules
open Microsoft.FSharp.Text
open Parser
open System.Text
open Lexer
open TypeInference


let type_infer (dataString : string) = 
    try
        let lexbuf = Lexing.LexBuffer<byte>.FromBytes(Encoding.ASCII.GetBytes dataString)
        let y = Parser.program Lexer.tokenize lexbuf
        List.iter (fun x -> printfn "%A \n %A\n" x (TypeInference.infer Env.empty 0 x)) y 
    with
        | LexingError(str) -> printfn "Lexing error: %s"str
        | TypeError(str) -> printfn "Type error: %s" str

let readLines filePath = System.IO.File.ReadAllText(filePath)

[<EntryPoint>]
let main(args) =
    type_infer (readLines args.[0]);
    0