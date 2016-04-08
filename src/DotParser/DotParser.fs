
# 2 "DotParser.fs"
module DotParserProject.DotParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "DotGrammar.yrd"

open DotParserProject.GraphData
open Option
open System.Collections.Generic
open QuickGraph

let mutable g: GraphData = null


# 19 "DotParser.fs"
type Token =
    | DIGRAPH of (string)
    | GRAPH of (string)
    | ID of (string)
    | LCURBRACE of (string)
    | RCURBRACE of (string)
    | RNGLR_EOF of (string)
    | SEMI of (string)
    | STRICT of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | DIGRAPH x -> box x
    | GRAPH x -> box x
    | ID x -> box x
    | LCURBRACE x -> box x
    | RCURBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | STRICT x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "graph"
    | 2 -> "graph_type"
    | 3 -> "stmt"
    | 4 -> "yard_exp_brackets_57"
    | 5 -> "yard_exp_brackets_58"
    | 6 -> "yard_many_27"
    | 7 -> "yard_opt_68"
    | 8 -> "yard_opt_69"
    | 9 -> "yard_opt_70"
    | 10 -> "yard_start_rule"
    | 11 -> "DIGRAPH"
    | 12 -> "GRAPH"
    | 13 -> "ID"
    | 14 -> "LCURBRACE"
    | 15 -> "RCURBRACE"
    | 16 -> "RNGLR_EOF"
    | 17 -> "SEMI"
    | 18 -> "STRICT"
    | _ -> ""

let tokenToNumber = function
    | DIGRAPH _ -> 11
    | GRAPH _ -> 12
    | ID _ -> 13
    | LCURBRACE _ -> 14
    | RCURBRACE _ -> 15
    | RNGLR_EOF _ -> 16
    | SEMI _ -> 17
    | STRICT _ -> 18

let isLiteral = function
    | DIGRAPH _ -> false
    | GRAPH _ -> false
    | ID _ -> false
    | LCURBRACE _ -> false
    | RCURBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | STRICT _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|5; 5; 4; 3; 9; 9; 2; 6; 6; 8; 8; 7; 7; 1; 10|]
let private rules = [|12; 11; 3; 8; 13; 18; 9; 5; 4; 6; 17; 13; 2; 7; 14; 6; 15; 1|]
let private rulesStart = [|0; 1; 2; 4; 5; 5; 6; 8; 8; 10; 10; 11; 11; 12; 17; 18|]
let startRule = 14

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 14; 18; 3; 13; 4; 5; 8; 11; 10; 6; 7; 9; 12; 15; 16; 17|]
let private small_gotos =
        [|4; 65536; 131073; 589826; 1179651; 131074; 458756; 851973; 196609; 917510; 262148; 196615; 262152; 393225; 851978; 327682; 524299; 1114124; 524292; 196615; 262152; 393229; 851978; 720897; 983054; 917507; 327695; 720912; 786449|]
let gotos = Array.zeroCreate 19
for i = 0 to 18 do
        gotos.[i] <- Array.zeroCreate 19
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|2,1|]; [|2,2|]; [|10,1|]; [|8,1|]; [|8,2|]; [|3,1|]; [|13,5|]; [|12,1|]; [|6,2|]; [|1,1|]; [|0,1|]; [|5,1|]|]
let private small_reduces =
        [|327682; 851968; 983040; 393218; 851969; 983041; 458754; 851970; 983042; 524289; 983043; 589825; 983044; 655363; 851973; 983045; 1114117; 786433; 1048582; 851969; 917511; 983042; 851976; 917512; 1048578; 851977; 917513; 1114114; 851978; 917514; 1179650; 720907; 786443|]
let reduces = Array.zeroCreate 19
for i = 0 to 18 do
        reduces.[i] <- Array.zeroCreate 19
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|4|]; [|11|]; [|7|]; [|9|]|]
let private small_zeroReduces =
        [|2; 720896; 786432; 131073; 917505; 262145; 983042; 327682; 851971; 983043; 524289; 983042|]
let zeroReduces = Array.zeroCreate 19
for i = 0 to 18 do
        zeroReduces.[i] <- Array.zeroCreate 19
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [1]
let private accStates = Array.zeroCreate 19
for i = 0 to 18 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 16
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_stmt * '_rnglr_type_yard_exp_brackets_57 * '_rnglr_type_yard_exp_brackets_58 * '_rnglr_type_yard_many_27 * '_rnglr_type_yard_opt_68 * '_rnglr_type_yard_opt_69 * '_rnglr_type_yard_opt_70 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                            false
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_58) 
# 180 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "DIGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                                              true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_58) 
# 200 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_69) g
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 27 "DotGrammar.yrd"
                                                                           
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_57) 
# 222 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (name) -> 
              _rnglr_cycle_res := (
                
# 32 "DotGrammar.yrd"
                                                  g.AddNode name 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 242 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 30 "DotGrammar.yrd"
                      None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_70) 
# 260 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRICT _rnglr_val -> [_rnglr_val] | a -> failwith "STRICT expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                        Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_70) 
# 280 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_70) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_58) s
               |> List.iter (fun (d) -> 
                _rnglr_cycle_res := (
                  
# 30 "DotGrammar.yrd"
                                                                         g <- new GraphData (d, not <| isSome s); g
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 29 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 302 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 27 "DotGrammar.yrd"
                                                    []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_27) 
# 320 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_57) g
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_27) g
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 27 "DotGrammar.yrd"
                                                        yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_27) 
# 342 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 27 "DotGrammar.yrd"
                                                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_69) 
# 360 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 27 "DotGrammar.yrd"
                                                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_69) 
# 380 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 27 "DotGrammar.yrd"
                                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_68) 
# 398 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 27 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_68) 
# 418 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_68) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LCURBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_yard_many_27) g
                   |> List.iter (fun (_) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RCURBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 27 "DotGrammar.yrd"
                                                                                                 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_graph) 
# 446 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 456 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 474 "DotParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_graph)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_graph_type)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_57)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_58)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_27)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_68)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_69)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_70)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
