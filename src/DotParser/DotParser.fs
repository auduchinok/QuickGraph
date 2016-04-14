
# 2 "DotParser.fs"
module DotParserProject.DotParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 3 "DotGrammar.yrd"

open DotParserProject.GraphData
open Option
open System.Collections.Generic
open System.Linq
open QuickGraph

# 17 "DotParser.fs"
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
    | 16 -> "yard_exp_brackets_61"
    | 17 -> "yard_exp_brackets_62"
    | 18 -> "yard_exp_brackets_63"
    | 19 -> "yard_exp_brackets_64"
    | 20 -> "yard_exp_brackets_65"
    | 21 -> "yard_exp_brackets_66"
    | 22 -> "yard_opt_111"
    | 23 -> "yard_opt_112"
    | 24 -> "yard_opt_113"
    | 25 -> "yard_opt_114"
    | 26 -> "yard_opt_115"
    | 27 -> "yard_opt_116"
    | 28 -> "yard_opt_117"
    | 29 -> "yard_opt_118"
    | 30 -> "yard_opt_119"
    | 31 -> "yard_opt_120"
    | 32 -> "yard_opt_121"
    | 33 -> "yard_start_rule"
    | 34 -> "ASSIGN"
    | 35 -> "COMMA"
    | 36 -> "DIEDGE"
    | 37 -> "DIGRAPH"
    | 38 -> "EDGE"
    | 39 -> "GRAPH"
    | 40 -> "ID"
    | 41 -> "LBRACE"
    | 42 -> "LBRACK"
    | 43 -> "NODE"
    | 44 -> "RBRACE"
    | 45 -> "RBRACK"
    | 46 -> "RNGLR_EOF"
    | 47 -> "SEMI"
    | 48 -> "STRICT"
    | 49 -> "SUBGRAPH"
    | _ -> ""

let tokenToNumber = function
    | ASSIGN _ -> 34
    | COMMA _ -> 35
    | DIEDGE _ -> 36
    | DIGRAPH _ -> 37
    | EDGE _ -> 38
    | GRAPH _ -> 39
    | ID _ -> 40
    | LBRACE _ -> 41
    | LBRACK _ -> 42
    | NODE _ -> 43
    | RBRACE _ -> 44
    | RBRACK _ -> 45
    | RNGLR_EOF _ -> 46
    | SEMI _ -> 47
    | STRICT _ -> 48
    | SUBGRAPH _ -> 49

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
let leftSide = [|21; 20; 20; 19; 19; 18; 18; 18; 17; 16; 16; 31; 31; 32; 32; 15; 10; 6; 12; 12; 30; 30; 4; 5; 11; 1; 29; 29; 28; 28; 0; 27; 27; 2; 3; 13; 13; 13; 24; 24; 26; 26; 25; 25; 14; 23; 23; 9; 22; 22; 8; 33|]
let private rules = [|49; 32; 38; 36; 47; 35; 39; 43; 38; 13; 25; 26; 39; 37; 21; 40; 31; 41; 14; 44; 40; 20; 12; 10; 15; 4; 6; 30; 12; 4; 10; 40; 34; 40; 0; 19; 1; 28; 29; 0; 42; 27; 45; 18; 2; 11; 5; 3; 17; 14; 47; 24; 48; 23; 16; 40; 9; 22; 41; 14; 44; 8|]
let private rulesStart = [|0; 2; 3; 4; 5; 6; 7; 8; 9; 12; 13; 14; 14; 15; 15; 16; 20; 21; 23; 24; 25; 25; 26; 28; 30; 31; 34; 34; 35; 35; 36; 39; 39; 40; 43; 45; 46; 47; 48; 48; 49; 49; 50; 50; 51; 52; 52; 53; 55; 55; 56; 61; 62|]
let startRule = 51

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 58; 62; 3; 57; 4; 5; 6; 7; 8; 9; 20; 55; 16; 23; 24; 17; 40; 18; 42; 43; 44; 45; 46; 10; 11; 13; 53; 54; 12; 52; 14; 15; 19; 50; 21; 49; 22; 41; 25; 26; 27; 28; 38; 33; 29; 30; 36; 37; 31; 32; 34; 35; 39; 47; 48; 51; 56; 59; 60; 61|]
let private small_gotos =
        [|4; 524288; 589825; 1507330; 3145731; 131074; 1441796; 2621445; 196609; 2686982; 262162; 196615; 327688; 655369; 720906; 786443; 851980; 917517; 983054; 1114127; 1179664; 1376273; 1572882; 2031635; 2490388; 2555925; 2621462; 2818071; 3211288; 589829; 262169; 393242; 1310747; 2359324; 2490397; 720902; 262174; 393242; 1310747; 1966111; 2359324; 2490397; 851975; 655392; 786465; 983054; 1376273; 2031635; 2621462; 3211288; 1179649; 2687010; 1245202; 196615; 327688; 655369; 720906; 786443; 851980; 917539; 983054; 1114127; 1179664; 1376273; 1572882; 2031635; 2490388; 2555925; 2621462; 2818071; 3211288; 1310722; 1638436; 3080229; 1376275; 196615; 327688; 655369; 720906; 786443; 851980; 917542; 983054; 1114127; 1179664; 1376273; 1572882; 1703975; 2031635; 2490388; 2555925; 2621462; 2818071; 3211288; 1572866; 131112; 2752553; 1703940; 42; 65579; 1769516; 2621485; 1835012; 1245230; 1835055; 2293808; 3080241; 1966084; 50; 65579; 1900595; 2621485; 2162689; 2228276; 2228225; 2621493; 2490369; 2949174; 3014658; 2097207; 2621496; 3276801; 2883641; 3604481; 2883642; 3801091; 1048635; 2424892; 2555965|]
let gotos = Array.zeroCreate 63
for i = 0 to 62 do
        gotos.[i] <- Array.zeroCreate 50
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
let private lists_reduces = [|[|37,1|]; [|36,1|]; [|18,1|]; [|24,1; 18,1|]; [|24,1|]; [|35,1|]; [|23,2|]; [|22,1|]; [|21,1|]; [|17,2|]; [|19,1|]; [|12,1|]; [|8,1|]; [|8,2|]; [|41,1|]; [|39,1|]; [|34,2|]; [|32,1|]; [|30,1|]; [|29,1|]; [|30,2|]; [|27,1|]; [|30,3|]; [|25,3|]; [|4,1|]; [|3,1|]; [|33,3|]; [|44,1|]; [|8,3|]; [|7,1|]; [|5,1|]; [|16,1|]; [|6,1|]; [|0,1|]; [|0,2|]; [|14,1|]; [|43,1|]; [|15,4|]; [|22,2|]; [|2,1|]; [|1,1|]; [|50,5|]; [|49,1|]; [|47,2|]; [|10,1|]; [|9,1|]; [|46,1|]|]
let private small_reduces =
        [|327688; 2490368; 2555904; 2621440; 2686976; 2818048; 2883584; 3080192; 3211264; 393224; 2490369; 2555905; 2621441; 2686977; 2818049; 2883585; 3080193; 3211265; 458761; 2359298; 2490371; 2555908; 2621444; 2686980; 2818052; 2883588; 3080196; 3211268; 524296; 2490373; 2555909; 2621445; 2686981; 2818053; 2883589; 3080197; 3211269; 655368; 2490374; 2555910; 2621446; 2686982; 2818054; 2883590; 3080198; 3211270; 720904; 2490375; 2555911; 2621447; 2686983; 2818055; 2883591; 3080199; 3211271; 786440; 2490376; 2555912; 2621448; 2686984; 2818056; 2883592; 3080200; 3211272; 917513; 2359298; 2490370; 2555906; 2621442; 2686978; 2818050; 2883586; 3080194; 3211266; 983049; 2359305; 2490377; 2555913; 2621449; 2686985; 2818057; 2883593; 3080201; 3211273; 1048585; 2359306; 2490378; 2555914; 2621450; 2686986; 2818058; 2883594; 3080202; 3211274; 1114113; 2686987; 1310721; 2883596; 1376257; 2883597; 1441793; 2883598; 1507329; 2883599; 1638408; 2490384; 2555920; 2621456; 2686992; 2818064; 2883600; 3080208; 3211280; 1769473; 2949137; 1835009; 2949138; 1900546; 2621459; 2949139; 1966081; 2949140; 2031617; 2949141; 2097153; 2949142; 2293764; 2293783; 2621463; 2949143; 3080215; 2359298; 2621464; 2949144; 2424834; 2621465; 2949145; 2555912; 2490394; 2555930; 2621466; 2687002; 2818074; 2883610; 3080218; 3211290; 2621441; 2883611; 2686977; 2883612; 2752513; 2752541; 2818049; 2752542; 2883593; 2359327; 2490399; 2555935; 2621471; 2687007; 2818079; 2883615; 3080223; 3211295; 2949121; 2752544; 3014657; 2687009; 3080193; 2687010; 3145729; 2687011; 3211271; 2490404; 2555940; 2621476; 2687012; 2818084; 2883620; 3211300; 3342345; 2359333; 2490405; 2555941; 2621477; 2687013; 2818085; 2883621; 3080229; 3211301; 3407880; 2490406; 2555942; 2621478; 2687014; 2818086; 2883622; 3080230; 3211302; 3473411; 2621479; 2687015; 3211303; 3538947; 2621480; 2687016; 3211304; 3670017; 3014697; 3735553; 2687018; 3866626; 2621483; 2687019; 3932162; 2621484; 2687020; 3997698; 2621485; 2687021; 4063234; 2424878; 2555950|]
let reduces = Array.zeroCreate 63
for i = 0 to 62 do
        reduces.[i] <- Array.zeroCreate 50
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
let private lists_zeroReduces = [|[|45|]; [|48|]; [|11|]; [|44; 38|]; [|20|]; [|42|]; [|44; 41; 40; 38|]; [|31|]; [|28|]; [|26|]; [|13|]|]
let private small_zeroReduces =
        [|2; 2424832; 2555904; 131073; 2686977; 262146; 2686978; 2883587; 720904; 2490372; 2555908; 2621444; 2686980; 2818052; 2883588; 3080196; 3211268; 851969; 2686978; 1245186; 2686978; 2883587; 1310727; 2490373; 2555909; 2621445; 2686981; 2818053; 2883589; 3211269; 1376258; 2686978; 2883590; 1703937; 2949127; 1835010; 2621448; 2949128; 1966081; 2949129; 3014657; 2686986|]
let zeroReduces = Array.zeroCreate 63
for i = 0 to 62 do
        zeroReduces.[i] <- Array.zeroCreate 50
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
let eofIndex = 46
let errorIndex = 7
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), [|new Family(41, new Nodes([|box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(31, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(42, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), [|new Family(41, new Nodes([|box (new AST(new Family(44, new Nodes([|box (new AST(new Family(38, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(31, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_a_list * '_rnglr_type_attr * '_rnglr_type_attr_list * '_rnglr_type_attr_stmt * '_rnglr_type_edge_rhs * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_nodes * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_61 * '_rnglr_type_yard_exp_brackets_62 * '_rnglr_type_yard_exp_brackets_63 * '_rnglr_type_yard_exp_brackets_64 * '_rnglr_type_yard_exp_brackets_65 * '_rnglr_type_yard_exp_brackets_66 * '_rnglr_type_yard_opt_111 * '_rnglr_type_yard_opt_112 * '_rnglr_type_yard_opt_113 * '_rnglr_type_yard_opt_114 * '_rnglr_type_yard_opt_115 * '_rnglr_type_yard_opt_116 * '_rnglr_type_yard_opt_117 * '_rnglr_type_yard_opt_118 * '_rnglr_type_yard_opt_119 * '_rnglr_type_yard_opt_120 * '_rnglr_type_yard_opt_121 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "SUBGRAPH expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_121) (g: GraphData)
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 64 "DotGrammar.yrd"
                                       
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_66) 
# 243 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) ->
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

               : '_rnglr_type_yard_exp_brackets_65) 
# 263 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) ->
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

               : '_rnglr_type_yard_exp_brackets_65) 
# 283 "DotParser.fs"
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
                
# 43 "DotGrammar.yrd"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_64) 
# 303 "DotParser.fs"
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
                
# 43 "DotGrammar.yrd"
                                                   
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_64) 
# 323 "DotParser.fs"
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
                
# 39 "DotGrammar.yrd"
                               "graph" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_63) 
# 343 "DotParser.fs"
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
                
# 39 "DotGrammar.yrd"
                                                  "node" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_63) 
# 363 "DotParser.fs"
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
                
# 39 "DotGrammar.yrd"
                                                                    "edge" 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_63) 
# 383 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt) g
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_114) (g: GraphData) s
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_115) (g: GraphData) s
                 |> List.iter (fun (s1) -> 
                  _rnglr_cycle_res := (
                    
# 30 "DotGrammar.yrd"
                                                                     if isSome s1 then s1.Value else s 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_62) 
# 407 "DotParser.fs"
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
                
# 27 "DotGrammar.yrd"
                                                  false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_61) 
# 427 "DotParser.fs"
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
                
# 27 "DotGrammar.yrd"
                                                                      true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_61) 
# 447 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_120) 
# 465 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_66) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 64 "DotGrammar.yrd"
                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 64 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_120) 
# 485 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 25 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_121) 
# 503 "DotParser.fs"
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
                
# 25 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_121) 
# 523 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_120) (g: GraphData)
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_stmt_list) (copyAttrs g)
                 |> List.iter (fun (s) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      
# 64 "DotGrammar.yrd"
                                                                                           addSubgraph g s 
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 63 "DotGrammar.yrd"
               : '_rnglr_type_subgraph) 
# 549 "DotParser.fs"
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
                                                     addNode g name 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 61 "DotGrammar.yrd"
               : '_rnglr_type_node_id) 
# 569 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_65) (d: GraphData * string list)
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_nodes) (fst d)
               |> List.iter (fun (n) -> 
                _rnglr_cycle_res := (
                  
# 59 "DotGrammar.yrd"
                                                                   addEdges (fst n) (snd d) (snd n) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 58 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 591 "DotParser.fs"
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
                
# 55 "DotGrammar.yrd"
                                                          n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 55 "DotGrammar.yrd"
               : '_rnglr_type_nodes) 
# 611 "DotParser.fs"
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
                
# 55 "DotGrammar.yrd"
                                                                                  n 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 55 "DotGrammar.yrd"
               : '_rnglr_type_nodes) 
# 631 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) -> fun e ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 53 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 53 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_119) 
# 649 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) -> fun e ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_rhs) (fst e, snd e)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 53 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 53 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_119) 
# 669 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (d: GraphData * string list) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeop) d
             |> List.iter (fun (e) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_119) (d: GraphData * string list) e
               |> List.iter (fun (r) -> 
                _rnglr_cycle_res := (
                  
# 53 "DotGrammar.yrd"
                                                                     if isSome r then r.Value else e 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 52 "DotGrammar.yrd"
               : '_rnglr_type_edge_rhs) 
# 691 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_nodes) g
             |> List.iter (fun (n) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_edge_rhs) (fst n, snd n)
               |> List.iter (fun (g1) -> 
                _rnglr_cycle_res := (
                  
# 50 "DotGrammar.yrd"
                                                                                            fst g1 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 50 "DotGrammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 713 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 48 "DotGrammar.yrd"
                                                               fst g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 48 "DotGrammar.yrd"
               : '_rnglr_type_node_stmt) 
# 733 "DotParser.fs"
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
                    
# 46 "DotGrammar.yrd"
                                             (k, v) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 46 "DotGrammar.yrd"
               : '_rnglr_type_attr) 
# 757 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 41 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_118) 
# 775 "DotParser.fs"
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
                
# 41 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_118) 
# 795 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
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
               : '_rnglr_type_yard_opt_117) 
# 813 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun a ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_64) a
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_117) 
# 833 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr) 
             |> List.iter (fun (a) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_117) a
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_yard_opt_118) a
                 |> List.iter (fun (l) -> 
                  _rnglr_cycle_res := (
                    
# 43 "DotGrammar.yrd"
                                                                       if isSome l then a :: l.Value else [a] 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 43 "DotGrammar.yrd"
               : '_rnglr_type_a_list) 
# 857 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 41 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_116) 
# 875 "DotParser.fs"
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
                
# 41 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_116) 
# 895 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACK expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_116) 
               |> List.iter (fun (a) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RBRACK _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACK expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 41 "DotGrammar.yrd"
                                                          if isSome a then a.Value else [] 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 41 "DotGrammar.yrd"
               : '_rnglr_type_attr_list) 
# 919 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_63) (g: GraphData)
             |> List.iter (fun (k) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_attr_list) 
               |> List.iter (fun (a) -> 
                _rnglr_cycle_res := (
                  
# 39 "DotGrammar.yrd"
                                                                                              addAttributes g k (Map.ofList a) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 38 "DotGrammar.yrd"
               : '_rnglr_type_attr_stmt) 
# 941 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 33 "DotGrammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 961 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edge_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 34 "DotGrammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 981 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_stmt) g
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 35 "DotGrammar.yrd"
                                        g1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 1001 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
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
               : '_rnglr_type_yard_opt_113) 
# 1019 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_62) (g: GraphData)
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                          Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_113) 
# 1039 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
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
               : '_rnglr_type_yard_opt_115) 
# 1057 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_stmt_list) s
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_115) 
# 1077 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
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
               : '_rnglr_type_yard_opt_114) 
# 1095 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) -> fun s ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMI _rnglr_val -> [_rnglr_val] | a -> failwith "SEMI expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_114) 
# 1115 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g: GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_113) (g: GraphData)
             |> List.iter (fun (g1) -> 
              _rnglr_cycle_res := (
                
# 30 "DotGrammar.yrd"
                                                                                                        if isSome g1 then g1.Value else g 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 29 "DotGrammar.yrd"
               : '_rnglr_type_stmt_list) 
# 1135 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
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
               : '_rnglr_type_yard_opt_112) 
# 1153 "DotParser.fs"
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
                
# 27 "DotGrammar.yrd"
                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_112) 
# 1173 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_112) 
             |> List.iter (fun (s) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_exp_brackets_61) s
               |> List.iter (fun (d) -> 
                _rnglr_cycle_res := (
                  
# 27 "DotGrammar.yrd"
                                                                                  emptyGraph d (isSome s) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 27 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 1195 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 25 "DotGrammar.yrd"
                                   None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_111) 
# 1213 "DotParser.fs"
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
                
# 25 "DotGrammar.yrd"
                                     Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_111) 
# 1233 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_111) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt_list) g
                   |> List.iter (fun (r) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 25 "DotGrammar.yrd"
                                                                                  r 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_graph) 
# 1261 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 25 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 1271 "DotParser.fs"
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
# 1289 "DotParser.fs"
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
      box ( fun (d: GraphData * string list) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_rhs)  (d: GraphData * string list) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_stmt)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * string list) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edgeop)  (d: GraphData * string list) ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_61)  s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_62)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_63)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_64)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * string list) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_65)  (d: GraphData * string list) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_66)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_111)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_112)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_113)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_114)  (g: GraphData) s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) -> fun s ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_115)  (g: GraphData) s ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_116)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_117)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun a ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_118)  a ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (d: GraphData * string list) -> fun e ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_119)  (d: GraphData * string list) e ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_120)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g: GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_121)  (g: GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
