
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
open System.Linq
open QuickGraph

let mutable g: GraphData = null


# 20 "DotParser.fs"
type Token =
    | DIGRAPH of (string)
    | EDGEOP of (string)
    | GRAPH of (string)
    | ID of (string)
    | LCURBRACE of (string)
    | RCURBRACE of (string)
    | RNGLR_EOF of (string)
    | SEMI of (string)
    | STRICT of (string)
    | SUBGRAPH of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | DIGRAPH x -> box x
    | EDGEOP x -> box x
    | GRAPH x -> box x
    | ID x -> box x
    | LCURBRACE x -> box x
    | RCURBRACE x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | STRICT x -> box x
    | SUBGRAPH x -> box x

let numToString = function
    | 0 -> "edge_rhs"
    | 1 -> "edge_stmt"
    | 2 -> "edgeop"
    | 3 -> "error"
    | 4 -> "graph"
    | 5 -> "graph_type"
    | 6 -> "node_id"
    | 7 -> "node_stmt"
    | 8 -> "stmt"
    | 9 -> "stmt_list"
    | 10 -> "subgraph"
    | 11 -> "yard_exp_brackets_150"
    | 12 -> "yard_exp_brackets_151"
    | 13 -> "yard_exp_brackets_152"
    | 14 -> "yard_exp_brackets_153"
    | 15 -> "yard_many_65"
    | 16 -> "yard_opt_217"
    | 17 -> "yard_opt_218"
    | 18 -> "yard_opt_219"
    | 19 -> "yard_opt_220"
    | 20 -> "yard_opt_221"
    | 21 -> "yard_opt_222"
    | 22 -> "yard_start_rule"
    | 23 -> "DIGRAPH"
    | 24 -> "EDGEOP"
    | 25 -> "GRAPH"
    | 26 -> "ID"
    | 27 -> "LCURBRACE"
    | 28 -> "RCURBRACE"
    | 29 -> "RNGLR_EOF"
    | 30 -> "SEMI"
    | 31 -> "STRICT"
    | 32 -> "SUBGRAPH"
    | _ -> ""

let tokenToNumber = function
    | DIGRAPH _ -> 23
    | EDGEOP _ -> 24
    | GRAPH _ -> 25
    | ID _ -> 26
    | LCURBRACE _ -> 27
    | RCURBRACE _ -> 28
    | RNGLR_EOF _ -> 29
    | SEMI _ -> 30
    | STRICT _ -> 31
    | SUBGRAPH _ -> 32

let isLiteral = function
    | DIGRAPH _ -> false
    | EDGEOP _ -> false
    | GRAPH _ -> false
    | ID _ -> false
    | LCURBRACE _ -> false
    | RCURBRACE _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | STRICT _ -> false
    | SUBGRAPH _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|14; 13; 12; 11; 11; 20; 20; 21; 21; 10; 2; 6; 19; 19; 0; 1; 7; 8; 8; 8; 15; 15; 18; 18; 9; 17; 17; 5; 16; 16; 4; 22|]
let private rules = [|32; 21; 2; 6; 8; 18; 25; 23; 14; 26; 20; 27; 9; 28; 24; 26; 0; 13; 19; 6; 0; 6; 7; 1; 10; 12; 15; 30; 15; 31; 17; 11; 26; 5; 16; 27; 9; 28; 4|]
let private rulesStart = [|0; 2; 4; 6; 7; 8; 8; 9; 9; 10; 14; 15; 16; 16; 17; 19; 21; 22; 23; 24; 25; 25; 27; 27; 28; 29; 29; 30; 32; 32; 33; 38; 39|]
let startRule = 31

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 34; 38; 3; 33; 4; 5; 6; 15; 16; 19; 21; 22; 23; 29; 25; 10; 30; 7; 8; 11; 14; 9; 12; 13; 17; 18; 20; 24; 26; 27; 28; 31; 32; 35; 36; 37|]
let private small_gotos =
        [|4; 262144; 327681; 1114114; 2031619; 131074; 1048580; 1703941; 196609; 1769478; 262156; 65543; 393224; 458761; 524298; 589835; 655372; 786445; 917518; 983055; 1310736; 1703953; 2097170; 393220; 19; 131092; 851989; 1572886; 524290; 393239; 1703953; 720901; 24; 131092; 851989; 1245209; 1572886; 1048578; 1179674; 1966107; 1245185; 1835036; 1441803; 65543; 393224; 458761; 524298; 655372; 786445; 917518; 983069; 1310736; 1703953; 2097170; 1638401; 1769502; 1703948; 65543; 393224; 458761; 524298; 589855; 655372; 786445; 917518; 983055; 1310736; 1703953; 2097170; 1769473; 1835040; 1966082; 1376289; 1703970; 2228227; 720931; 1507364; 1638437|]
let gotos = Array.zeroCreate 39
for i = 0 to 38 do
        gotos.[i] <- Array.zeroCreate 33
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
let private lists_reduces = [|[|18,1|]; [|16,1|]; [|15,2|]; [|1,2|]; [|11,1|]; [|14,1|]; [|13,1|]; [|14,2|]; [|10,1|]; [|17,1|]; [|2,1|]; [|2,2|]; [|23,1|]; [|30,5|]; [|19,1|]; [|21,1|]; [|6,1|]; [|21,2|]; [|9,4|]; [|24,1|]; [|0,1|]; [|0,2|]; [|8,1|]; [|29,1|]; [|27,2|]; [|4,1|]; [|3,1|]; [|26,1|]|]
let private small_reduces =
        [|327685; 1703936; 1769472; 1835008; 1966080; 2097152; 393221; 1703937; 1769473; 1835009; 1966081; 2097153; 458757; 1703938; 1769474; 1835010; 1966082; 2097154; 589830; 1572867; 1703939; 1769475; 1835011; 1966083; 2097155; 655366; 1572868; 1703940; 1769476; 1835012; 1966084; 2097156; 720901; 1703941; 1769477; 1835013; 1966085; 2097157; 786437; 1703942; 1769478; 1835014; 1966086; 2097158; 851973; 1703943; 1769479; 1835015; 1966087; 2097159; 917505; 1703944; 983045; 1703945; 1769481; 1835017; 1966089; 2097161; 1048580; 1703946; 1769482; 1835018; 2097162; 1114116; 1703947; 1769483; 1835019; 2097163; 1179652; 1703948; 1769484; 1835020; 2097164; 1310721; 1900557; 1376261; 1703950; 1769486; 1835022; 1966094; 2097166; 1441793; 1835023; 1507329; 1769488; 1572865; 1835025; 1835013; 1703954; 1769490; 1835026; 1966098; 2097170; 1900545; 1835027; 1966081; 1769492; 2031617; 1769493; 2097153; 1769494; 2162689; 1769495; 2293762; 1703960; 1769496; 2359298; 1703961; 1769497; 2424834; 1703962; 1769498; 2490370; 1507355; 1638427|]
let reduces = Array.zeroCreate 39
for i = 0 to 38 do
        reduces.[i] <- Array.zeroCreate 33
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
let private lists_zeroReduces = [|[|25|]; [|28|]; [|5|]; [|24; 20|]; [|12|]; [|22|]; [|20|]; [|7|]|]
let private small_zeroReduces =
        [|2; 1507328; 1638400; 131073; 1769473; 262146; 1769474; 1835011; 720901; 1703940; 1769476; 1835012; 1966084; 2097156; 1048580; 1703941; 1769477; 1835013; 2097157; 1441794; 1769474; 1835014; 1703938; 1769474; 1835011; 1966081; 1769479|]
let zeroReduces = Array.zeroCreate 39
for i = 0 to 38 do
        zeroReduces.[i] <- Array.zeroCreate 33
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
let private accStates = Array.zeroCreate 39
for i = 0 to 38 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 29
let errorIndex = 3
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(24, new Nodes([|box (new AST(new Family(20, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(24, new Nodes([|box (new AST(new Family(20, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_edge_rhs * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_150 * '_rnglr_type_yard_exp_brackets_151 * '_rnglr_type_yard_exp_brackets_152 * '_rnglr_type_yard_exp_brackets_153 * '_rnglr_type_yard_many_65 * '_rnglr_type_yard_opt_217 * '_rnglr_type_yard_opt_218 * '_rnglr_type_yard_opt_219 * '_rnglr_type_yard_opt_220 * '_rnglr_type_yard_opt_221 * '_rnglr_type_yard_opt_222 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "SUBGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_222) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 51 "DotGrammar.yrd"
                                          
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_153) 
# 205 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: (GraphData * string list)) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeop) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_node_id) g
               |> List.iter (fun (n) -> 
                _rnglr_cycle_res := (
                  
# 44 "DotGrammar.yrd"
                                                   g.AddEdge d n 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_152) 
# 227 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_219) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 31 "DotGrammar.yrd"
                                                                    
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_151) 
# 249 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 29 "DotGrammar.yrd"
                                            false
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_150) 
# 269 "DotParser.fs"
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
                
# 29 "DotGrammar.yrd"
                                                              true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_150) 
# 289 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 51 "DotGrammar.yrd"
                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 51 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_221) 
# 307 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_153) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 51 "DotGrammar.yrd"
                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 51 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_221) 
# 327 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 26 "DotGrammar.yrd"
                                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_222) 
# 345 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 26 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_222) 
# 365 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_221) (g: GraphData)
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LCURBRACE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_stmt_list) g
                 |> List.iter (fun (_) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RCURBRACE expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 51 "DotGrammar.yrd"
                                                                                        g.GetGraph().Vertices.ToList() 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 50 "DotGrammar.yrd"
               : '_rnglr_type_subgraph) 
# 391 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGEOP _rnglr_val -> [_rnglr_val] | a -> failwith "EDGEOP expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 48 "DotGrammar.yrd"
                                 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 48 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 411 "DotParser.fs"
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
                
# 46 "DotGrammar.yrd"
                                                     g.AddNode name 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 46 "DotGrammar.yrd"
               : '_rnglr_type_node_id) 
# 431 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: (GraphData * string list)) -> fun nn ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 44 "DotGrammar.yrd"
                                                                  None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 44 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_220) 
# 449 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: (GraphData * string list)) -> fun nn ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_rhs) (g, nn)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 44 "DotGrammar.yrd"
                                                                    Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 44 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_220) 
# 469 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: (GraphData * string list)) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_152) (d: (GraphData * string list))
             |> List.iter (fun (nn) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_220) (d: (GraphData * string list)) nn
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 44 "DotGrammar.yrd"
                                                                                              
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 43 "DotGrammar.yrd"
               : '_rnglr_type_edge_rhs) 
# 491 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (n) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_edge_rhs) (g, n)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 41 "DotGrammar.yrd"
                                                                                   
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 513 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 39 "DotGrammar.yrd"
                                                            
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 39 "DotGrammar.yrd"
               : '_rnglr_type_node_stmt) 
# 533 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_stmt) g
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 34 "DotGrammar.yrd"
                                     
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 553 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_stmt) g
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 35 "DotGrammar.yrd"
                                     
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 573 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subgraph) (new GraphData(g.GetGraph().IsDirected, not <| g.GetGraph().AllowParallelEdges))
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 37 "DotGrammar.yrd"
                      (* should inherit all parent's attributes; return nodes here *) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 593 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 31 "DotGrammar.yrd"
                                             []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_65) 
# 611 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_151) (g: GraphData)
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_65) (g: GraphData)
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 31 "DotGrammar.yrd"
                                                 yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_65) 
# 633 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 31 "DotGrammar.yrd"
                                                        None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_219) 
# 651 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                          Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_219) 
# 671 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_many_65) (g: GraphData)
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                                         
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_stmt_list) 
# 691 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 29 "DotGrammar.yrd"
                      None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 29 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_218) 
# 709 "DotParser.fs"
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
                
# 29 "DotGrammar.yrd"
                        Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_218) 
# 729 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_218) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_150) s
               |> List.iter (fun (d) -> 
                _rnglr_cycle_res := (
                  
# 29 "DotGrammar.yrd"
                                                                         g <- new GraphData (d, not <| isSome s); g
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 28 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 751 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 26 "DotGrammar.yrd"
                                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_217) 
# 769 "DotParser.fs"
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
                
# 26 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_217) 
# 789 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_217) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LCURBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt_list) g
                   |> List.iter (fun (_) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RCURBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 26 "DotGrammar.yrd"
                                                                                         
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_graph) 
# 817 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 827 "DotParser.fs"
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
# 845 "DotParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: (GraphData * string list)) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_rhs)  (d: (GraphData * string list)) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edgeop)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_id)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt_list)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_subgraph)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_150)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_151)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: (GraphData * string list)) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_152)  (d: (GraphData * string list)) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_153)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_65)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_217)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_218)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_219)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: (GraphData * string list)) -> fun nn ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_220)  (d: (GraphData * string list)) nn ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_221)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_222)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
