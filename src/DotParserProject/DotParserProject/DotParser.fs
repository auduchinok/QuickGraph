
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

let mutable data = new GraphData()

# 18 "DotParser.fs"
type Token =
    | ASSIGN of (string)
    | COL of (string)
    | COMMA of (string)
    | DIEDGEOP of (string)
    | DIGRAPH of (string)
    | EDGE of (string)
    | EDGEOP of (string)
    | GRAPH of (string)
    | ID of (string)
    | LCURBRACE of (string)
    | LSQBRACE of (string)
    | NODE of (string)
    | RCURBRACE of (string)
    | RNGLR_EOF of (string)
    | RSQBRACE of (string)
    | SEMI of (string)
    | STRICT of (string)
    | SUBGR of (string)

let genLiteral (str : string) posStart posEnd =
    match str.ToLower() with
    | x -> failwithf "Literal %s undefined" x
let tokenData = function
    | ASSIGN x -> box x
    | COL x -> box x
    | COMMA x -> box x
    | DIEDGEOP x -> box x
    | DIGRAPH x -> box x
    | EDGE x -> box x
    | EDGEOP x -> box x
    | GRAPH x -> box x
    | ID x -> box x
    | LCURBRACE x -> box x
    | LSQBRACE x -> box x
    | NODE x -> box x
    | RCURBRACE x -> box x
    | RNGLR_EOF x -> box x
    | RSQBRACE x -> box x
    | SEMI x -> box x
    | STRICT x -> box x
    | SUBGR x -> box x

let numToString = function
    | 0 -> "a_list"
    | 1 -> "attr_list"
    | 2 -> "attr_stmt"
    | 3 -> "edgeRHS"
    | 4 -> "edge_stmt"
    | 5 -> "edgeop"
    | 6 -> "error"
    | 7 -> "graph"
    | 8 -> "graph_type"
    | 9 -> "node_id"
    | 10 -> "node_stmt"
    | 11 -> "port"
    | 12 -> "stmt"
    | 13 -> "stmt_list"
    | 14 -> "subgraph"
    | 15 -> "yard_exp_brackets_326"
    | 16 -> "yard_exp_brackets_327"
    | 17 -> "yard_exp_brackets_328"
    | 18 -> "yard_exp_brackets_329"
    | 19 -> "yard_exp_brackets_330"
    | 20 -> "yard_exp_brackets_331"
    | 21 -> "yard_exp_brackets_332"
    | 22 -> "yard_many_8"
    | 23 -> "yard_many_9"
    | 24 -> "yard_opt_799"
    | 25 -> "yard_opt_800"
    | 26 -> "yard_opt_801"
    | 27 -> "yard_opt_802"
    | 28 -> "yard_opt_803"
    | 29 -> "yard_opt_804"
    | 30 -> "yard_opt_805"
    | 31 -> "yard_opt_806"
    | 32 -> "yard_opt_807"
    | 33 -> "yard_opt_808"
    | 34 -> "yard_opt_809"
    | 35 -> "yard_opt_810"
    | 36 -> "yard_opt_811"
    | 37 -> "yard_start_rule"
    | 38 -> "ASSIGN"
    | 39 -> "COL"
    | 40 -> "COMMA"
    | 41 -> "DIEDGEOP"
    | 42 -> "DIGRAPH"
    | 43 -> "EDGE"
    | 44 -> "EDGEOP"
    | 45 -> "GRAPH"
    | 46 -> "ID"
    | 47 -> "LCURBRACE"
    | 48 -> "LSQBRACE"
    | 49 -> "NODE"
    | 50 -> "RCURBRACE"
    | 51 -> "RNGLR_EOF"
    | 52 -> "RSQBRACE"
    | 53 -> "SEMI"
    | 54 -> "STRICT"
    | 55 -> "SUBGR"
    | _ -> ""

let tokenToNumber = function
    | ASSIGN _ -> 38
    | COL _ -> 39
    | COMMA _ -> 40
    | DIEDGEOP _ -> 41
    | DIGRAPH _ -> 42
    | EDGE _ -> 43
    | EDGEOP _ -> 44
    | GRAPH _ -> 45
    | ID _ -> 46
    | LCURBRACE _ -> 47
    | LSQBRACE _ -> 48
    | NODE _ -> 49
    | RCURBRACE _ -> 50
    | RNGLR_EOF _ -> 51
    | RSQBRACE _ -> 52
    | SEMI _ -> 53
    | STRICT _ -> 54
    | SUBGR _ -> 55

let isLiteral = function
    | ASSIGN _ -> false
    | COL _ -> false
    | COMMA _ -> false
    | DIEDGEOP _ -> false
    | DIGRAPH _ -> false
    | EDGE _ -> false
    | EDGEOP _ -> false
    | GRAPH _ -> false
    | ID _ -> false
    | LCURBRACE _ -> false
    | LSQBRACE _ -> false
    | NODE _ -> false
    | RCURBRACE _ -> false
    | RNGLR_EOF _ -> false
    | RSQBRACE _ -> false
    | SEMI _ -> false
    | STRICT _ -> false
    | SUBGR _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|21; 20; 20; 19; 19; 18; 18; 17; 16; 16; 16; 15; 35; 35; 36; 36; 14; 5; 5; 11; 34; 34; 9; 33; 33; 10; 32; 32; 3; 31; 31; 4; 30; 30; 29; 29; 0; 23; 23; 28; 28; 1; 2; 12; 12; 12; 12; 12; 22; 22; 27; 27; 13; 26; 26; 25; 25; 8; 8; 24; 24; 7; 37|]
let private rules = [|55; 36; 9; 14; 9; 14; 40; 53; 48; 28; 52; 45; 49; 43; 12; 27; 21; 46; 35; 47; 13; 50; 44; 41; 39; 46; 11; 46; 34; 1; 9; 33; 3; 5; 20; 32; 1; 19; 3; 31; 0; 18; 46; 38; 46; 29; 30; 17; 23; 0; 23; 16; 1; 10; 4; 2; 46; 38; 46; 14; 15; 22; 53; 22; 54; 54; 25; 45; 26; 42; 46; 8; 24; 47; 13; 50; 7|]
let private rulesStart = [|0; 2; 3; 4; 5; 6; 7; 8; 11; 12; 13; 14; 16; 16; 17; 17; 18; 22; 23; 24; 26; 26; 27; 29; 29; 30; 32; 32; 33; 36; 36; 37; 40; 40; 41; 41; 42; 47; 47; 49; 49; 50; 51; 53; 54; 55; 56; 59; 60; 60; 62; 62; 63; 64; 64; 65; 65; 66; 68; 70; 70; 71; 76; 77|]
let startRule = 62

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 70; 72; 74; 3; 69; 4; 5; 6; 7; 26; 27; 30; 32; 33; 34; 36; 48; 53; 49; 54; 55; 56; 63; 64; 8; 9; 24; 25; 11; 10; 12; 13; 15; 14; 16; 17; 18; 19; 22; 23; 20; 21; 28; 29; 31; 68; 35; 37; 40; 46; 47; 38; 39; 41; 42; 43; 67; 44; 45; 50; 51; 52; 57; 58; 59; 61; 60; 62; 65; 66; 71; 73|]
let private small_gotos =
        [|5; 458752; 524289; 1638402; 1703939; 3538948; 131074; 1572869; 3014662; 196609; 3080199; 262162; 131080; 262153; 589834; 655371; 786444; 851981; 917518; 983055; 1048592; 1245201; 1376274; 1441811; 2293780; 2818069; 2949142; 3014679; 3211288; 3604505; 458757; 65562; 1114139; 1507356; 2162717; 3145758; 589827; 1114139; 1507359; 3145758; 720899; 32; 1835041; 3014690; 851969; 3407907; 983041; 2490404; 1048577; 3014693; 1114116; 1179686; 1900583; 2621480; 3473449; 1245187; 42; 1966123; 3014690; 1769474; 1769516; 3473453; 1966081; 3276846; 2162705; 131080; 262153; 589834; 655371; 786444; 917518; 983055; 1048592; 1245201; 1376274; 1441839; 2293780; 2818069; 2949142; 3014679; 3211288; 3604505; 2228228; 65584; 1114139; 1507356; 3145758; 2359300; 196657; 327730; 2687027; 2883636; 2424837; 65589; 1114139; 1507356; 2031670; 3145758; 2621447; 589879; 917560; 1310777; 1376274; 2293780; 3014714; 3604505; 2818053; 196667; 327730; 2097212; 2687027; 2883636; 3211265; 3080253; 3276818; 131080; 262153; 589834; 655371; 786444; 852030; 917518; 983055; 1048592; 1245201; 1376274; 1441811; 2293780; 2818069; 2949142; 3014679; 3211288; 3604505; 3342337; 3276863; 3670020; 720960; 2228289; 2490434; 2555971; 3866625; 3014724; 3997697; 3014725; 4194306; 2359366; 3014727; 4390915; 720960; 2228289; 2555971; 4587521; 2949192; 4718593; 2752585|]
let gotos = Array.zeroCreate 75
for i = 0 to 74 do
        gotos.[i] <- Array.zeroCreate 56
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
let private lists_reduces = [|[|45,1|]; [|44,1|]; [|3,1|]; [|25,1|]; [|24,1|]; [|38,1|]; [|38,2|]; [|40,1|]; [|7,3|]; [|36,3|]; [|35,1|]; [|36,4|]; [|33,1|]; [|36,5|]; [|5,1|]; [|6,1|]; [|41,1|]; [|25,2|]; [|43,1|]; [|11,1|]; [|11,2|]; [|51,1|]; [|61,5|]; [|4,1|]; [|47,1|]; [|49,1|]; [|42,1|]; [|42,2|]; [|31,2|]; [|30,1|]; [|31,3|]; [|1,1|]; [|2,1|]; [|28,2|]; [|27,1|]; [|28,3|]; [|18,1|]; [|17,1|]; [|13,1|]; [|16,4|]; [|52,1|]; [|10,1|]; [|8,1|]; [|22,1|]; [|21,1|]; [|22,2|]; [|46,3|]; [|19,2|]; [|9,1|]; [|0,1|]; [|0,2|]; [|15,1|]; [|49,2|]; [|60,1|]; [|57,2|]; [|58,2|]; [|54,1|]; [|56,1|]|]
let private small_reduces =
        [|327688; 2818048; 2949120; 3014656; 3080192; 3211264; 3276800; 3473408; 3604480; 393224; 2818049; 2949121; 3014657; 3080193; 3211265; 3276801; 3473409; 3604481; 458762; 2686978; 2818051; 2883586; 2949123; 3014659; 3080195; 3211267; 3276803; 3473411; 3604483; 524296; 2818052; 2949124; 3014660; 3080196; 3211268; 3276804; 3473412; 3604484; 589832; 2818053; 2949125; 3014661; 3080197; 3211269; 3276805; 3473413; 3604485; 655368; 2818054; 2949126; 3014662; 3080198; 3211270; 3276806; 3473414; 3604486; 786433; 3407879; 917513; 2818056; 2949128; 3014664; 3080200; 3145736; 3211272; 3276808; 3473416; 3604488; 1114113; 3407881; 1179650; 3014666; 3407882; 1245185; 3407883; 1310721; 3407884; 1376257; 3407885; 1441794; 3014670; 3407886; 1507330; 3014671; 3407887; 1572872; 2818064; 2949136; 3014672; 3080208; 3211280; 3276816; 3473424; 3604496; 1638408; 2818065; 2949137; 3014673; 3080209; 3211281; 3276817; 3473425; 3604497; 1703944; 2818066; 2949138; 3014674; 3080210; 3211282; 3276818; 3473426; 3604498; 1769479; 2818067; 2949139; 3014675; 3080211; 3211283; 3276819; 3604499; 1835015; 2818068; 2949140; 3014676; 3080212; 3211284; 3276820; 3604500; 1900551; 2818069; 2949141; 3014677; 3080213; 3211285; 3276821; 3604501; 2031617; 3342358; 2097162; 2686999; 2818072; 2883607; 2949144; 3014680; 3080216; 3211288; 3276824; 3473432; 3604504; 2162689; 3276825; 2228232; 2818074; 2949146; 3014682; 3080218; 3211290; 3276826; 3473434; 3604506; 2293768; 2818075; 2949147; 3014683; 3080219; 3211291; 3276827; 3473435; 3604507; 2424840; 2818076; 2949148; 3014684; 3080220; 3211292; 3276828; 3473436; 3604508; 2490376; 2818077; 2949149; 3014685; 3080221; 3211293; 3276829; 3473437; 3604509; 2555912; 2818078; 2949150; 3014686; 3080222; 3211294; 3276830; 3473438; 3604510; 2686987; 2687007; 2818079; 2883615; 2949151; 3014687; 3080223; 3145759; 3211295; 3276831; 3473439; 3604511; 2752523; 2687008; 2818080; 2883616; 2949152; 3014688; 3080224; 3145760; 3211296; 3276832; 3473440; 3604512; 2818057; 2818081; 2949153; 3014689; 3080225; 3145761; 3211297; 3276833; 3473441; 3604513; 2883593; 2818082; 2949154; 3014690; 3080226; 3145762; 3211298; 3276834; 3473442; 3604514; 2949129; 2818083; 2949155; 3014691; 3080227; 3145763; 3211299; 3276835; 3473443; 3604515; 3014659; 3014692; 3080228; 3604516; 3080195; 3014693; 3080229; 3604517; 3145729; 3080230; 3407883; 2687015; 2818087; 2883623; 2949159; 3014695; 3080231; 3145767; 3211303; 3276839; 3473447; 3604519; 3473409; 3276840; 3538953; 2818089; 2949161; 3014697; 3080233; 3145769; 3211305; 3276841; 3473449; 3604521; 3604489; 2818090; 2949162; 3014698; 3080234; 3145770; 3211306; 3276842; 3473450; 3604522; 3670027; 2687019; 2818091; 2883627; 2949163; 3014699; 3080235; 3145771; 3211307; 3276843; 3473451; 3604523; 3735563; 2687020; 2818092; 2883628; 2949164; 3014700; 3080236; 3145772; 3211308; 3276844; 3473452; 3604524; 3801099; 2687021; 2818093; 2883629; 2949165; 3014701; 3080237; 3145773; 3211309; 3276845; 3473453; 3604525; 3932168; 2818094; 2949166; 3014702; 3080238; 3211310; 3276846; 3473454; 3604526; 4063243; 2687023; 2818095; 2883631; 2949167; 3014703; 3080239; 3145775; 3211311; 3276847; 3473455; 3604527; 4128777; 2818096; 2949168; 3014704; 3080240; 3145776; 3211312; 3276848; 3473456; 3604528; 4194305; 3080241; 4259841; 3080242; 4325377; 3080243; 4390923; 2687019; 2818091; 2883627; 2949163; 3014699; 3080235; 3145771; 3211307; 3276843; 3473451; 3604523; 4456449; 3276852; 4521985; 3080245; 4653058; 3014710; 3080246; 4784130; 3014711; 3080247; 4849666; 2752568; 2949177|]
let reduces = Array.zeroCreate 75
for i = 0 to 74 do
        reduces.[i] <- Array.zeroCreate 56
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
let private lists_zeroReduces = [|[|53|]; [|55|]; [|59|]; [|12|]; [|52; 48|]; [|41; 37; 24; 23|]; [|37|]; [|39|]; [|34|]; [|32|]; [|50|]; [|48|]; [|41; 37|]; [|41; 37; 30; 29|]; [|26|]; [|20|]; [|14|]|]
let private small_zeroReduces =
        [|2; 2752512; 2949121; 131073; 3080194; 262146; 3080195; 3276804; 458760; 2818053; 2949125; 3014661; 3080197; 3211269; 3276805; 3473413; 3604485; 589832; 2818054; 2949126; 3014662; 3080198; 3211270; 3276806; 3473414; 3604486; 720897; 3407879; 1114114; 3014664; 3407880; 1245185; 3407881; 1769479; 2818058; 2949130; 3014666; 3080202; 3211274; 3276810; 3604490; 2162690; 3080195; 3276811; 2228232; 2818060; 2949132; 3014668; 3080204; 3211276; 3276812; 3473420; 3604492; 2424840; 2818061; 2949133; 3014669; 3080205; 3211277; 3276813; 3473421; 3604493; 2621441; 3080195; 2818057; 2818062; 2949134; 3014670; 3080206; 3145742; 3211278; 3276814; 3473422; 3604494; 3276802; 3080195; 3276804; 3670027; 2686991; 2818063; 2883599; 2949135; 3014671; 3080207; 3145743; 3211279; 3276815; 3473423; 3604495; 4194305; 3080208; 4390923; 2686991; 2818063; 2883599; 2949135; 3014671; 3080207; 3145743; 3211279; 3276815; 3473423; 3604495|]
let zeroReduces = Array.zeroCreate 75
for i = 0 to 74 do
        zeroReduces.[i] <- Array.zeroCreate 56
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
let private accStates = Array.zeroCreate 75
for i = 0 to 74 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 51
let errorIndex = 6
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(63, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([|box (new AST(new Family(48, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(50, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(39, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), [|new Family(30, new Nodes([|box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), [|new Family(24, new Nodes([|box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|null; new Tree<_>(null,box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(63, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([|box (new AST(new Family(48, new Nodes([||])), null))|])), null)), null); null; null; null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(48, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(50, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(39, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), [|new Family(30, new Nodes([|box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), [|new Family(24, new Nodes([|box (new AST(new Family(41, new Nodes([|box (new AST(new Family(37, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(20, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_a_list * '_rnglr_type_attr_list * '_rnglr_type_attr_stmt * '_rnglr_type_edgeRHS * '_rnglr_type_edge_stmt * '_rnglr_type_edgeop * '_rnglr_type_error * '_rnglr_type_graph * '_rnglr_type_graph_type * '_rnglr_type_node_id * '_rnglr_type_node_stmt * '_rnglr_type_port * '_rnglr_type_stmt * '_rnglr_type_stmt_list * '_rnglr_type_subgraph * '_rnglr_type_yard_exp_brackets_326 * '_rnglr_type_yard_exp_brackets_327 * '_rnglr_type_yard_exp_brackets_328 * '_rnglr_type_yard_exp_brackets_329 * '_rnglr_type_yard_exp_brackets_330 * '_rnglr_type_yard_exp_brackets_331 * '_rnglr_type_yard_exp_brackets_332 * '_rnglr_type_yard_many_8 * '_rnglr_type_yard_many_9 * '_rnglr_type_yard_opt_799 * '_rnglr_type_yard_opt_800 * '_rnglr_type_yard_opt_801 * '_rnglr_type_yard_opt_802 * '_rnglr_type_yard_opt_803 * '_rnglr_type_yard_opt_804 * '_rnglr_type_yard_opt_805 * '_rnglr_type_yard_opt_806 * '_rnglr_type_yard_opt_807 * '_rnglr_type_yard_opt_808 * '_rnglr_type_yard_opt_809 * '_rnglr_type_yard_opt_810 * '_rnglr_type_yard_opt_811 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_332) 
# 246 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_331) 
# 256 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_331) 
# 266 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )

               : '_rnglr_type_yard_exp_brackets_330) 
# 276 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )

               : '_rnglr_type_yard_exp_brackets_330) 
# 286 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_329) 
# 296 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_329) 
# 306 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )

               : '_rnglr_type_yard_exp_brackets_328) 
# 316 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
             |> List.iter (fun (t) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                t
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_327) 
# 336 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with NODE _rnglr_val -> [_rnglr_val] | a -> failwith "NODE expected, but %A found" a )
             |> List.iter (fun (t) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                            t
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_327) 
# 356 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EDGE _rnglr_val -> [_rnglr_val] | a -> failwith "EDGE expected, but %A found" a )
             |> List.iter (fun (t) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                                         t
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_yard_exp_brackets_327) 
# 376 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )

               : '_rnglr_type_yard_exp_brackets_326) 
# 386 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 58 "DotGrammar.yrd"
                         None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 58 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_810) 
# 404 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_332) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 58 "DotGrammar.yrd"
                           Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 58 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_810) 
# 424 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 24 "DotGrammar.yrd"
                                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_811) 
# 442 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 24 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_811) 
# 462 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 58 "DotGrammar.yrd"
               : '_rnglr_type_subgraph) 
# 472 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 56 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 482 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 56 "DotGrammar.yrd"
               : '_rnglr_type_edgeop) 
# 492 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 54 "DotGrammar.yrd"
               : '_rnglr_type_port) 
# 502 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 52 "DotGrammar.yrd"
                           None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 52 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_809) 
# 520 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_port) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 52 "DotGrammar.yrd"
                             Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 52 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_809) 
# 540 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ID _rnglr_val -> [_rnglr_val] | a -> failwith "ID expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_809) 
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 52 "DotGrammar.yrd"
                                       ID 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 52 "DotGrammar.yrd"
               : '_rnglr_type_node_id) 
# 562 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) -> fun name ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "DotGrammar.yrd"
                                                             None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 40 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_808) 
# 580 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) -> fun name ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 40 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_808) 
# 600 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_node_id) 
             |> List.iter (fun (name) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_808) (g:GraphData) name
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 50 "DotGrammar.yrd"
                                                                         g.AddNode name 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 50 "DotGrammar.yrd"
               : '_rnglr_type_node_stmt) 
# 622 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 46 "DotGrammar.yrd"
                                                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 46 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_807) 
# 640 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_edgeRHS) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 46 "DotGrammar.yrd"
                                                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 46 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_807) 
# 660 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 48 "DotGrammar.yrd"
               : '_rnglr_type_edgeRHS) 
# 670 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 40 "DotGrammar.yrd"
                                                             None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 40 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_806) 
# 688 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_attr_list) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 40 "DotGrammar.yrd"
                                                               Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 40 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_806) 
# 708 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 46 "DotGrammar.yrd"
               : '_rnglr_type_edge_stmt) 
# 718 "DotParser.fs"
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
# 736 "DotParser.fs"
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
# 756 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
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
               : '_rnglr_type_yard_opt_804) 
# 774 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_329) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 44 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 44 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_804) 
# 794 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 44 "DotGrammar.yrd"
               : '_rnglr_type_a_list) 
# 804 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 42 "DotGrammar.yrd"
                          []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_9) 
# 822 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_328) 
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_9) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 42 "DotGrammar.yrd"
                              yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_9) 
# 844 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_803) 
# 862 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_803) 
# 882 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          []
            )
# 42 "DotGrammar.yrd"
               : '_rnglr_type_attr_list) 
# 892 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_327) (g:GraphData)
             |> List.iter (fun (t) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_attr_list) 
               |> List.iter (fun (l) -> 
                _rnglr_cycle_res := (
                  
# 40 "DotGrammar.yrd"
                                                                             g.AddDefaultAttributes t l 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 39 "DotGrammar.yrd"
               : '_rnglr_type_attr_stmt) 
# 914 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 924 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 934 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 944 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 954 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 32 "DotGrammar.yrd"
               : '_rnglr_type_stmt) 
# 964 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 30 "DotGrammar.yrd"
                                           []
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_8) 
# 982 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_exp_brackets_326) (g:GraphData)
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_many_8) (g:GraphData)
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 30 "DotGrammar.yrd"
                                               yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_yard_many_8) 
# 1004 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
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
               : '_rnglr_type_yard_opt_802) 
# 1022 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
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
               : '_rnglr_type_yard_opt_802) 
# 1042 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun (g:GraphData) ->
          []
            )
# 30 "DotGrammar.yrd"
               : '_rnglr_type_stmt_list) 
# 1052 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_801) 
# 1070 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_801) 
# 1090 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_800) 
# 1108 "DotParser.fs"
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
               : '_rnglr_type_yard_opt_800) 
# 1128 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_800) 
             |> List.iter (fun (s) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with GRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "GRAPH expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 27 "DotGrammar.yrd"
                                             data.graph <- new UndirectedGraph<_,_>    (isSome s); data 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 1150 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_801) 
             |> List.iter (fun (s) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with DIGRAPH _rnglr_val -> [_rnglr_val] | a -> failwith "DIGRAPH expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 28 "DotGrammar.yrd"
                                             data.graph <- new BidirectionalGraph<_,_> (isSome s); data 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 26 "DotGrammar.yrd"
               : '_rnglr_type_graph_type) 
# 1172 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( fun g ->
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 24 "DotGrammar.yrd"
                                    None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_799) 
# 1190 "DotParser.fs"
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
                
# 24 "DotGrammar.yrd"
                                      Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_yard_opt_799) 
# 1210 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_graph_type) 
             |> List.iter (fun (g) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_799) g
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "LCURBRACE expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_stmt_list) g
                   |> List.iter (fun (_) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RCURBRACE _rnglr_val -> [_rnglr_val] | a -> failwith "RCURBRACE expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 24 "DotGrammar.yrd"
                                                                                        g 
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_graph) 
# 1238 "DotParser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_graph) 
            )
# 24 "DotGrammar.yrd"
               : '_rnglr_type_yard_start_rule) 
# 1248 "DotParser.fs"
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
# 1266 "DotParser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_a_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_attr_stmt)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edgeRHS)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_edge_stmt)  (g:GraphData) ) |> List.concat));
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
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_id)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_node_stmt)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_port)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_stmt_list)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_subgraph)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_326)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_327)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_328)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_329)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_330)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_331)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_exp_brackets_332)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_8)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_many_9)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun g ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_799)  g ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_800)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_801)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_802)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_803)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_804)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_805)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_806)  (g:GraphData) ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_807)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( fun (g:GraphData) -> fun name ->
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_808)  (g:GraphData) name ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_809)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_810)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_811)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
