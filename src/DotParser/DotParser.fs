
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
    | 11 -> "yard_exp_brackets_141"
    | 12 -> "yard_exp_brackets_142"
    | 13 -> "yard_exp_brackets_143"
    | 14 -> "yard_many_62"
    | 15 -> "yard_opt_201"
    | 16 -> "yard_opt_202"
    | 17 -> "yard_opt_203"
    | 18 -> "yard_opt_204"
    | 19 -> "yard_opt_205"
    | 20 -> "yard_start_rule"
    | 21 -> "DIGRAPH"
    | 22 -> "EDGEOP"
    | 23 -> "GRAPH"
    | 24 -> "ID"
    | 25 -> "LCURBRACE"
    | 26 -> "RCURBRACE"
    | 27 -> "RNGLR_EOF"
    | 28 -> "SEMI"
    | 29 -> "STRICT"
    | 30 -> "SUBGRAPH"
    | _ -> ""

let tokenToNumber = function
    | DIGRAPH _ -> 21
    | EDGEOP _ -> 22
    | GRAPH _ -> 23
    | ID _ -> 24
    | LCURBRACE _ -> 25
    | RCURBRACE _ -> 26
    | RNGLR_EOF _ -> 27
    | SEMI _ -> 28
    | STRICT _ -> 29
    | SUBGRAPH _ -> 30

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
let leftSide = [|13; 12; 11; 11; 18; 18; 19; 19; 10; 2; 6; 0; 1; 7; 8; 8; 8; 14; 14; 17; 17; 9; 16; 16; 5; 15; 15; 4; 20|]
let private rules = [|30; 19; 8; 17; 23; 21; 13; 24; 18; 25; 9; 26; 22; 24; 2; 6; 6; 0; 6; 7; 1; 10; 12; 14; 28; 14; 29; 16; 11; 24; 5; 15; 25; 9; 26; 4|]
let private rulesStart = [|0; 2; 4; 5; 6; 6; 7; 7; 8; 12; 13; 14; 16; 18; 19; 20; 21; 22; 22; 24; 24; 25; 26; 26; 27; 29; 29; 30; 35; 36|]
let startRule = 28

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 31; 35; 3; 30; 4; 5; 6; 12; 13; 16; 18; 19; 20; 26; 22; 10; 27; 7; 8; 11; 9; 14; 15; 17; 21; 23; 24; 25; 28; 29; 32; 33; 34|]
let private small_gotos =
        [|4; 262144; 327681; 1048578; 1900547; 131074; 983044; 1572869; 196609; 1638406; 262156; 65543; 393224; 458761; 524298; 589835; 655372; 786445; 851982; 917519; 1179664; 1572881; 1966098; 393219; 19; 131092; 1441813; 524290; 393238; 1572881; 851970; 1114135; 1835032; 1048577; 1703961; 1245195; 65543; 393224; 458761; 524298; 655372; 786445; 851982; 917530; 1179664; 1572881; 1966098; 1441793; 1638427; 1507340; 65543; 393224; 458761; 524298; 589852; 655372; 786445; 851982; 917519; 1179664; 1572881; 1966098; 1572865; 1703965; 1769474; 1245214; 1572895; 2031619; 720928; 1376289; 1507362|]
let gotos = Array.zeroCreate 36
for i = 0 to 35 do
        gotos.[i] <- Array.zeroCreate 31
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
let private lists_reduces = [|[|15,1|]; [|13,1|]; [|12,2|]; [|11,2|]; [|10,1|]; [|9,1|]; [|14,1|]; [|1,1|]; [|1,2|]; [|20,1|]; [|27,5|]; [|16,1|]; [|18,1|]; [|5,1|]; [|18,2|]; [|8,4|]; [|21,1|]; [|0,1|]; [|0,2|]; [|7,1|]; [|26,1|]; [|24,2|]; [|3,1|]; [|2,1|]; [|23,1|]|]
let private small_reduces =
        [|327685; 1572864; 1638400; 1703936; 1835008; 1966080; 393221; 1572865; 1638401; 1703937; 1835009; 1966081; 458757; 1572866; 1638402; 1703938; 1835010; 1966082; 589829; 1572867; 1638403; 1703939; 1835011; 1966083; 655366; 1441796; 1572868; 1638404; 1703940; 1835012; 1966084; 720897; 1572869; 786437; 1572870; 1638406; 1703942; 1835014; 1966086; 851972; 1572871; 1638407; 1703943; 1966087; 917508; 1572872; 1638408; 1703944; 1966088; 983044; 1572873; 1638409; 1703945; 1966089; 1114113; 1769482; 1179653; 1572875; 1638411; 1703947; 1835019; 1966091; 1245185; 1703948; 1310721; 1638413; 1376257; 1703950; 1638405; 1572879; 1638415; 1703951; 1835023; 1966095; 1703937; 1703952; 1769473; 1638417; 1835009; 1638418; 1900545; 1638419; 1966081; 1638420; 2097154; 1572885; 1638421; 2162690; 1572886; 1638422; 2228226; 1572887; 1638423; 2293762; 1376280; 1507352|]
let reduces = Array.zeroCreate 36
for i = 0 to 35 do
        reduces.[i] <- Array.zeroCreate 31
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
let private lists_zeroReduces = [|[|22|]; [|25|]; [|4|]; [|21; 17|]; [|19|]; [|17|]; [|6|]|]
let private small_zeroReduces =
        [|2; 1376256; 1507328; 131073; 1638401; 262146; 1638402; 1703939; 851972; 1572868; 1638404; 1703940; 1966084; 1245186; 1638402; 1703941; 1507330; 1638402; 1703939; 1769473; 1638406|]
let zeroReduces = Array.zeroCreate 36
for i = 0 to 35 do
        zeroReduces.[i] <- Array.zeroCreate 31
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
let private accStates = Array.zeroCreate 36
for i = 0 to 35 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 27
let errorIndex = 3
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([|box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([|box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_edge_rhs * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_141 * '_rnglr_type_yard_exp_brackets_142 * '_rnglr_type_yard_exp_brackets_143 * '_rnglr_type_yard_many_62 * '_rnglr_type_yard_opt_201 * '_rnglr_type_yard_opt_202 * '_rnglr_type_yard_opt_203 * '_rnglr_type_yard_opt_204 * '_rnglr_type_yard_opt_205 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "SUBGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_205) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 50 "DotGrammar.yrd"
                                          
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_143) 
# 203 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_203) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 31 "DotGrammar.yrd"
                                                                    
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_142) 
# 225 "DotParser.fs"
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

               : '_rnglr_type_yard_exp_brackets_141) 
# 245 "DotParser.fs"
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

               : '_rnglr_type_yard_exp_brackets_141) 
# 265 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 50 "DotGrammar.yrd"
                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 50 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_204) 
# 283 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_143) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 50 "DotGrammar.yrd"
                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 50 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_204) 
# 303 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_205) 
# 321 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_205) 
# 341 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_204) (g: GraphData)
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LCURBRACE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_stmt_list) g
                 |> List.iter (fun (_) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RCURBRACE expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 50 "DotGrammar.yrd"
                                                                                        g.GetGraph().Vertices.ToList() 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 49 "DotGrammar.yrd"
               : '_rnglr_type_subgraph) 
# 367 "DotParser.fs"
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
                
# 47 "DotGrammar.yrd"
                                 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 47 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 387 "DotParser.fs"
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
                
# 45 "DotGrammar.yrd"
                                                     g.AddNode name 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 45 "DotGrammar.yrd"
               : '_rnglr_type_node_id) 
# 407 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeop) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_node_id) g
               |> List.iter (fun (n) -> 
                _rnglr_cycle_res := (
                  
# 43 "DotGrammar.yrd"
                                                                      n (* should add edges here, not in edge_stmt *) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 43 "DotGrammar.yrd"
               : '_rnglr_type_edge_rhs) 
# 429 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (n1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_edge_rhs) g
               |> List.iter (fun (n2) -> 
                _rnglr_cycle_res := (
                  
# 41 "DotGrammar.yrd"
                                                                                  g.AddEdge n1 n2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 451 "DotParser.fs"
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
# 471 "DotParser.fs"
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
# 491 "DotParser.fs"
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
# 511 "DotParser.fs"
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
# 531 "DotParser.fs"
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
               : '_rnglr_type_yard_many_62) 
# 549 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_142) (g: GraphData)
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_62) (g: GraphData)
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 31 "DotGrammar.yrd"
                                                 yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_62) 
# 571 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_203) 
# 589 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_203) 
# 609 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_many_62) (g: GraphData)
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                                         
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_stmt_list) 
# 629 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_202) 
# 647 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_202) 
# 667 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_202) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_141) s
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
# 689 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_201) 
# 707 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_201) 
# 727 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_201) g
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
# 755 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 765 "DotParser.fs"
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
# 783 "DotParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_rhs)  (g: GraphData) ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_141)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_142)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_143)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_62)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_201)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_202)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_203)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_204)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_205)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
