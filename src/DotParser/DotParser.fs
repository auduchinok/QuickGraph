
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

let addEdge (d: GraphData * Node list) (n2: Node list) =
    let g, n1 = d
    g.AddEdge n1 n2

# 21 "DotParser.fs"
type Token =
    | ASSIGN of (string)
    | COMMA of (string)
    | DIEDGE of (string)
    | DIGRAPH of (string)
    | EDGE of (string)
    | GRAPH of (string)
    | ID of (string)
    | LBRACE of (string)
    | LBRACK of (string)
    | NODE of (string)
    | RBRACE of (string)
    | RBRACK of (string)
    | RNGLR_EOF of (string)
    | SEMI of (string)
    | STRICT of (string)
    | SUBGRAPH of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | ASSIGN x -> box x
    | COMMA x -> box x
    | DIEDGE x -> box x
    | DIGRAPH x -> box x
    | EDGE x -> box x
    | GRAPH x -> box x
    | ID x -> box x
    | LBRACE x -> box x
    | LBRACK x -> box x
    | NODE x -> box x
    | RBRACE x -> box x
    | RBRACK x -> box x
    | RNGLR_EOF x -> box x
    | SEMI x -> box x
    | STRICT x -> box x
    | SUBGRAPH x -> box x

let numToString = function
    | 0 -> "a_list"
    | 1 -> "attr"
    | 2 -> "attr_list"
    | 3 -> "attr_stmt"
    | 4 -> "edge_rhs"
    | 5 -> "edge_stmt"
    | 6 -> "edgeop"
    | 7 -> "error"
    | 8 -> "graph"
    | 9 -> "graph_type"
    | 10 -> "node_id"
    | 11 -> "node_stmt"
    | 12 -> "nodes"
    | 13 -> "stmt"
    | 14 -> "stmt_list"
    | 15 -> "subgraph"
    | 16 -> "yard_exp_brackets_430"
    | 17 -> "yard_exp_brackets_431"
    | 18 -> "yard_exp_brackets_432"
    | 19 -> "yard_exp_brackets_433"
    | 20 -> "yard_exp_brackets_434"
    | 21 -> "yard_opt_800"
    | 22 -> "yard_opt_801"
    | 23 -> "yard_opt_802"
    | 24 -> "yard_opt_803"
    | 25 -> "yard_opt_804"
    | 26 -> "yard_opt_805"
    | 27 -> "yard_opt_806"
    | 28 -> "yard_opt_807"
    | 29 -> "yard_opt_808"
    | 30 -> "yard_opt_809"
    | 31 -> "yard_opt_810"
    | 32 -> "yard_start_rule"
    | 33 -> "ASSIGN"
    | 34 -> "COMMA"
    | 35 -> "DIEDGE"
    | 36 -> "DIGRAPH"
    | 37 -> "EDGE"
    | 38 -> "GRAPH"
    | 39 -> "ID"
    | 40 -> "LBRACE"
    | 41 -> "LBRACK"
    | 42 -> "NODE"
    | 43 -> "RBRACE"
    | 44 -> "RBRACK"
    | 45 -> "RNGLR_EOF"
    | 46 -> "SEMI"
    | 47 -> "STRICT"
    | 48 -> "SUBGRAPH"
    | _ -> ""

let tokenToNumber = function
    | ASSIGN _ -> 33
    | COMMA _ -> 34
    | DIEDGE _ -> 35
    | DIGRAPH _ -> 36
    | EDGE _ -> 37
    | GRAPH _ -> 38
    | ID _ -> 39
    | LBRACE _ -> 40
    | LBRACK _ -> 41
    | NODE _ -> 42
    | RBRACE _ -> 43
    | RBRACK _ -> 44
    | RNGLR_EOF _ -> 45
    | SEMI _ -> 46
    | STRICT _ -> 47
    | SUBGRAPH _ -> 48

let isLiteral = function
    | ASSIGN _ -> false
    | COMMA _ -> false
    | DIEDGE _ -> false
    | DIGRAPH _ -> false
    | EDGE _ -> false
    | GRAPH _ -> false
    | ID _ -> false
    | LBRACE _ -> false
    | LBRACK _ -> false
    | NODE _ -> false
    | RBRACE _ -> false
    | RBRACK _ -> false
    | RNGLR_EOF _ -> false
    | SEMI _ -> false
    | STRICT _ -> false
    | SUBGRAPH _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|20; 19; 19; 18; 18; 18; 17; 16; 16; 30; 30; 31; 31; 15; 10; 6; 6; 12; 12; 29; 29; 4; 5; 11; 1; 28; 28; 27; 27; 0; 26; 26; 2; 3; 13; 13; 13; 13; 23; 23; 25; 25; 24; 24; 14; 22; 22; 9; 21; 21; 8; 32|]
let private rules = [|48; 31; 46; 34; 38; 42; 37; 13; 24; 25; 38; 36; 20; 39; 30; 40; 14; 43; 39; 37; 35; 10; 15; 4; 6; 12; 29; 12; 4; 10; 39; 33; 39; 0; 19; 1; 27; 28; 0; 41; 26; 44; 18; 2; 11; 5; 3; 15; 17; 14; 46; 23; 47; 22; 16; 39; 9; 21; 40; 14; 43; 8|]
let private rulesStart = [|0; 2; 3; 4; 5; 6; 7; 10; 11; 12; 12; 13; 13; 14; 18; 19; 20; 21; 22; 23; 23; 24; 27; 29; 30; 33; 33; 34; 34; 35; 38; 38; 39; 42; 44; 45; 46; 47; 48; 48; 49; 49; 50; 50; 51; 52; 52; 53; 55; 55; 56; 61; 62|]
let startRule = 51

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 58; 62; 3; 57; 4; 5; 6; 7; 8; 9; 22; 55; 25; 26; 27; 19; 43; 20; 45; 46; 47; 48; 49; 10; 11; 16; 17; 12; 13; 18; 14; 15; 21; 53; 23; 52; 24; 44; 28; 29; 30; 31; 41; 36; 32; 33; 39; 40; 34; 35; 37; 38; 42; 50; 51; 54; 56; 59; 60; 61|]
let private small_gotos =
        [|4; 524288; 589825; 1441794; 3080195; 131074; 1376260; 2555909; 196609; 2621446; 262162; 196615; 327688; 655369; 720906; 786443; 851980; 917517; 983054; 1114127; 1179664; 1310737; 1507346; 1966099; 2424852; 2490389; 2555926; 2752535; 3145752; 589828; 262169; 393242; 2293787; 2424860; 720903; 655389; 786462; 983071; 1310737; 1966099; 2555926; 3145752; 851973; 262176; 393242; 1900577; 2293787; 2424860; 1310721; 2621474; 1376274; 196615; 327688; 655369; 720906; 786443; 851980; 917539; 983054; 1114127; 1179664; 1310737; 1507346; 1966099; 2424852; 2490389; 2555926; 2752535; 3145752; 1441794; 1572900; 3014693; 1507347; 196615; 327688; 655369; 720906; 786443; 851980; 917542; 983054; 1114127; 1179664; 1310737; 1507346; 1638439; 1966099; 2424852; 2490389; 2555926; 2752535; 3145752; 1769474; 131112; 2687017; 1900548; 42; 65579; 1703980; 2555949; 2031620; 1245230; 1769519; 2228272; 3014705; 2162692; 50; 65579; 1835059; 2555949; 2359297; 2162740; 2424833; 2555957; 2686977; 2883638; 3211266; 2031671; 2555960; 3473409; 2818105; 3604481; 2818106; 3801091; 1048635; 2359356; 2490429|]
let gotos = Array.zeroCreate 63
for i = 0 to 62 do
        gotos.[i] <- Array.zeroCreate 49
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
let private lists_reduces = [|[|36,1|]; [|35,1|]; [|17,1|]; [|23,1; 17,1|]; [|23,1|]; [|34,1|]; [|22,2|]; [|21,2|]; [|20,1|]; [|21,3|]; [|16,1|]; [|15,1|]; [|18,1|]; [|10,1|]; [|6,1|]; [|6,2|]; [|41,1|]; [|37,1; 18,1|]; [|37,1|]; [|39,1|]; [|33,2|]; [|31,1|]; [|29,1|]; [|28,1|]; [|29,2|]; [|26,1|]; [|29,3|]; [|24,3|]; [|2,1|]; [|1,1|]; [|32,3|]; [|44,1|]; [|6,3|]; [|5,1|]; [|3,1|]; [|14,1|]; [|4,1|]; [|0,1|]; [|0,2|]; [|12,1|]; [|43,1|]; [|13,4|]; [|50,5|]; [|49,1|]; [|47,2|]; [|8,1|]; [|7,1|]; [|46,1|]|]
let private small_reduces =
        [|327688; 2424832; 2490368; 2555904; 2621440; 2752512; 2818048; 3014656; 3145728; 393224; 2424833; 2490369; 2555905; 2621441; 2752513; 2818049; 3014657; 3145729; 458761; 2293762; 2424835; 2490372; 2555908; 2621444; 2752516; 2818052; 3014660; 3145732; 524296; 2424837; 2490373; 2555909; 2621445; 2752517; 2818053; 3014661; 3145733; 655368; 2424838; 2490374; 2555910; 2621446; 2752518; 2818054; 3014662; 3145734; 786441; 2293762; 2424834; 2490370; 2555906; 2621442; 2752514; 2818050; 3014658; 3145730; 851976; 2424839; 2490375; 2555911; 2621447; 2752519; 2818055; 3014663; 3145735; 917512; 2424840; 2490376; 2555912; 2621448; 2752520; 2818056; 3014664; 3145736; 983048; 2424841; 2490377; 2555913; 2621449; 2752521; 2818057; 3014665; 3145737; 1048579; 2555914; 2621450; 3145738; 1114115; 2555915; 2621451; 3145739; 1179657; 2293772; 2424844; 2490380; 2555916; 2621452; 2752524; 2818060; 3014668; 3145740; 1245185; 2621453; 1441793; 2818062; 1507329; 2818063; 1572865; 2818064; 1638409; 2293772; 2424849; 2490386; 2555922; 2621458; 2752530; 2818066; 3014674; 3145746; 1703937; 2818067; 1835016; 2424852; 2490388; 2555924; 2621460; 2752532; 2818068; 3014676; 3145748; 1966081; 2883605; 2031617; 2883606; 2097154; 2555927; 2883607; 2162689; 2883608; 2228225; 2883609; 2293761; 2883610; 2490372; 2228251; 2555931; 2883611; 3014683; 2555906; 2555932; 2883612; 2621442; 2555933; 2883613; 2752520; 2424862; 2490398; 2555934; 2621470; 2752542; 2818078; 3014686; 3145758; 2818049; 2818079; 2883585; 2818080; 2949121; 2687009; 3014657; 2687010; 3080201; 2293795; 2424867; 2490403; 2555939; 2621475; 2752547; 2818083; 3014691; 3145763; 3145729; 2687012; 3211265; 2621477; 3276801; 2621478; 3342337; 2621479; 3407879; 2424872; 2490408; 2555944; 2621480; 2752552; 2818088; 3145768; 3538953; 2293801; 2424873; 2490409; 2555945; 2621481; 2752553; 2818089; 3014697; 3145769; 3670017; 2949162; 3735553; 2621483; 3866626; 2555948; 2621484; 3932162; 2555949; 2621485; 3997698; 2555950; 2621486; 4063234; 2359343; 2490415|]
let reduces = Array.zeroCreate 63
for i = 0 to 62 do
        reduces.[i] <- Array.zeroCreate 49
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
let private lists_zeroReduces = [|[|45|]; [|48|]; [|9|]; [|44; 38|]; [|19|]; [|42|]; [|44; 41; 40; 38|]; [|30|]; [|27|]; [|25|]; [|11|]|]
let private small_zeroReduces =
        [|2; 2359296; 2490368; 131073; 2621441; 262146; 2621442; 2818051; 720897; 2621442; 851976; 2424836; 2490372; 2555908; 2621444; 2752516; 2818052; 3014660; 3145732; 1376258; 2621442; 2818051; 1441799; 2424837; 2490373; 2555909; 2621445; 2752517; 2818053; 3145733; 1507330; 2621442; 2818054; 1900545; 2883591; 2031618; 2555912; 2883592; 2162689; 2883593; 3211265; 2621450|]
let zeroReduces = Array.zeroCreate 63
for i = 0 to 62 do
        zeroReduces.[i] <- Array.zeroCreate 49
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
let private accStates = Array.zeroCreate 63
for i = 0 to 62 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 45
let errorIndex = 7
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), [|new Family(41, new Nodes([|box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), [|new Family(41, new Nodes([|box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(9, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_a_list * '_rnglr_type_attr * '_rnglr_type_attr_list * '_rnglr_type_attr_stmt * '_rnglr_type_edge_rhs * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_nodes * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_430 * '_rnglr_type_yard_exp_brackets_431 * '_rnglr_type_yard_exp_brackets_432 * '_rnglr_type_yard_exp_brackets_433 * '_rnglr_type_yard_exp_brackets_434 * '_rnglr_type_yard_opt_800 * '_rnglr_type_yard_opt_801 * '_rnglr_type_yard_opt_802 * '_rnglr_type_yard_opt_803 * '_rnglr_type_yard_opt_804 * '_rnglr_type_yard_opt_805 * '_rnglr_type_yard_opt_806 * '_rnglr_type_yard_opt_807 * '_rnglr_type_yard_opt_808 * '_rnglr_type_yard_opt_809 * '_rnglr_type_yard_opt_810 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "SUBGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_810) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 64 "DotGrammar.yrd"
                                       
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_434) 
# 246 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 44 "DotGrammar.yrd"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_433) 
# 266 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 44 "DotGrammar.yrd"
                                                   
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_433) 
# 286 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                               "graph" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_432) 
# 306 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NODE _rnglr_val -> [_rnglr_val] | a -> failwith "NODE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                                  "node" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_432) 
# 326 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGE _rnglr_val -> [_rnglr_val] | a -> failwith "EDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                                                    "edge" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_432) 
# 346 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (r) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_803) (g: GraphData) r
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_804) (g: GraphData) r
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 31 "DotGrammar.yrd"
                                                                                        
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_431) 
# 370 "DotParser.fs"
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

               : '_rnglr_type_yard_exp_brackets_430) 
# 390 "DotParser.fs"
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

               : '_rnglr_type_yard_exp_brackets_430) 
# 410 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 64 "DotGrammar.yrd"
                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 64 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_809) 
# 428 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_434) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 64 "DotGrammar.yrd"
                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 64 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_809) 
# 448 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
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
               : '_rnglr_type_yard_opt_810) 
# 466 "DotParser.fs"
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
                
# 27 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_810) 
# 486 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_809) (g: GraphData)
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_stmt_list) (GraphData(g))
                 |> List.iter (fun (r) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 64 "DotGrammar.yrd"
                                                                                            g.AddSubgraph(r) 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 63 "DotGrammar.yrd"
               : '_rnglr_type_subgraph) 
# 512 "DotParser.fs"
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
                
# 61 "DotGrammar.yrd"
                                                     g.AddNode name 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 61 "DotGrammar.yrd"
               : '_rnglr_type_node_id) 
# 532 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGE _rnglr_val -> [_rnglr_val] | a -> failwith "EDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 59 "DotGrammar.yrd"
                               
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 59 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 552 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DIEDGE _rnglr_val -> [_rnglr_val] | a -> failwith "DIEDGE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 59 "DotGrammar.yrd"
                                            
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 59 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 572 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 56 "DotGrammar.yrd"
                                                          n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 56 "DotGrammar.yrd"
               : '_rnglr_type_nodes) 
# 592 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subgraph) g
             |> List.iter (fun (n) -> 
              _rnglr_cycle_res := (
                
# 56 "DotGrammar.yrd"
                                                                                  n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 56 "DotGrammar.yrd"
               : '_rnglr_type_nodes) 
# 612 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * Node list) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 54 "DotGrammar.yrd"
                                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 54 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_808) 
# 630 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * Node list) -> fun n ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_rhs) (fst d, n)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 54 "DotGrammar.yrd"
                                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 54 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_808) 
# 650 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * Node list) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeop) 
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_nodes) (fst d)
               |> List.iter (fun (n) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_808) (d: GraphData * Node list) n
                 |> List.iter (fun (r) -> 
                  _rnglr_cycle_res := (
                    
# 54 "DotGrammar.yrd"
                                                                               addEdge d n 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 53 "DotGrammar.yrd"
               : '_rnglr_type_edge_rhs) 
# 674 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_nodes) g
             |> List.iter (fun (n) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_edge_rhs) (g, n)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 51 "DotGrammar.yrd"
                                                                                 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 51 "DotGrammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 696 "DotParser.fs"
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
                
# 49 "DotGrammar.yrd"
                                                            
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 49 "DotGrammar.yrd"
               : '_rnglr_type_node_stmt) 
# 716 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (k) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ASSIGN _rnglr_val -> [_rnglr_val] | a -> failwith "ASSIGN expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
                 |> List.iter (fun (v) -> 
                  _rnglr_cycle_res := (
                    
# 47 "DotGrammar.yrd"
                                             (k, v) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 47 "DotGrammar.yrd"
               : '_rnglr_type_attr) 
# 740 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 42 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_807) 
# 758 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_a_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 42 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_807) 
# 778 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
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
               : '_rnglr_type_yard_opt_806) 
# 796 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_433) a
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                           Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_806) 
# 816 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr) 
             |> List.iter (fun (a) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_806) a
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_807) a
                 |> List.iter (fun (l) -> 
                  _rnglr_cycle_res := (
                    
# 44 "DotGrammar.yrd"
                                                                       if isSome l then a :: l.Value else [a] 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 44 "DotGrammar.yrd"
               : '_rnglr_type_a_list) 
# 840 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 42 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_805) 
# 858 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_a_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 42 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_805) 
# 878 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACK expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_805) 
               |> List.iter (fun (a) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACK expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 42 "DotGrammar.yrd"
                                                          if isSome a then a.Value else [] 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_attr_list) 
# 902 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_432) (g: GraphData)
             |> List.iter (fun (k) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_attr_list) 
               |> List.iter (fun (a) -> 
                _rnglr_cycle_res := (
                  
# 40 "DotGrammar.yrd"
                                                                                              g.AddAttributes k a 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 39 "DotGrammar.yrd"
               : '_rnglr_type_attr_stmt) 
# 924 "DotParser.fs"
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
                                     g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 944 "DotParser.fs"
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
                                     g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 964 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_stmt) g
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 36 "DotGrammar.yrd"
                                     g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 984 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subgraph) g
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 37 "DotGrammar.yrd"
                                     g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 33 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 1004 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_802) 
# 1022 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_431) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_802) 
# 1042 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun r ->
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
               : '_rnglr_type_yard_opt_804) 
# 1060 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun r ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt_list) r
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                                  Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_804) 
# 1080 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun r ->
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
               : '_rnglr_type_yard_opt_803) 
# 1098 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun r ->
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
               : '_rnglr_type_yard_opt_803) 
# 1118 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_802) (g: GraphData)
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 31 "DotGrammar.yrd"
                                                                                         g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 31 "DotGrammar.yrd"
               : '_rnglr_type_stmt_list) 
# 1138 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_801) 
# 1156 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_801) 
# 1176 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_801) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_430) s
               |> List.iter (fun (d) -> 
                _rnglr_cycle_res := (
                  
# 29 "DotGrammar.yrd"
                                                                                  GraphData (d, isSome s) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 29 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 1198 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_800) 
# 1216 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_800) 
# 1236 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_800) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt_list) g
                   |> List.iter (fun (r) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 27 "DotGrammar.yrd"
                                                                                  r 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_graph) 
# 1264 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 1274 "DotParser.fs"
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
# 1292 "DotParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_a_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * Node list) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_rhs)  (d: GraphData * Node list) ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_nodes)  (g: GraphData) ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_430)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_431)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_432)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_433)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_434)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_800)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_801)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_802)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun r ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_803)  (g: GraphData) r ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun r ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_804)  (g: GraphData) r ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_805)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_806)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_807)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * Node list) -> fun n ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_808)  (d: GraphData * Node list) n ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_809)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_810)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
