module DotParser

open Yard.Generators.RNGLR.Parser    
open Yard.Generators.RNGLR.AST
open DotParserProject.DotParser // todo: replace with Gen.DotParser
open DotParserProject.Gen.DotLexer
open DotParserProject.GraphData
open System.Collections.Generic
open QuickGraph

let parse (v: string -> Dictionary<_,_> -> _) str : IMutableVertexAndEdgeSet<_,_> =
    let translateArgs = {
            tokenToRange = fun _ -> Unchecked.defaultof<_>, Unchecked.defaultof<_>
            zeroPosition = Unchecked.defaultof<_>
            clearAST = false
            filterEpsilons = false
        }

    let lexbuf = Lexing.LexBuffer<_>.FromString str
    let tokens = seq { while not lexbuf.IsPastEndOfStream do yield tokenize lexbuf }

    let parsedGraphDataList : GraphData list = 
        match buildAst tokens with
        | Error (pos, token, msg, _, _) -> failwithf "Error on position %d, token %A: %s" pos token msg
        | Success (ast, errors) ->
//            ast.PrintAst()
            translate translateArgs ast errors
    
//    printfn "%A" <| List.length parsedGraphDataList 

    match parsedGraphDataList with
    | graphData :: _ -> graphData.Graph
    | [] -> failwith "Parser returned no data"
