module DotParser.Tests.Tests

open FsUnit
open NUnit.Framework
open DotParser
open System.Collections
open System.Linq
open QuickGraph


let name n _ = n
let nameWithAttr n a = (n, a)
let parseNames = DotParser.parse name

let isDirected (g: IMutableVertexAndEdgeSet<_,_>) = g.IsDirected
let isStrict (g: IMutableVertexAndEdgeSet<_,_>) = not g.AllowParallelEdges
let isEmpty (g: IMutableVertexAndEdgeSet<_,_>) = g.IsVerticesEmpty

let shouldContainVertices names (g: IMutableVertexAndEdgeSet<_,_>) =
    List.forall (fun n -> g.Vertices.Contains n) names |> should be True

let printGraph (g: IMutableVertexAndEdgeSet<_,_>) =
    printfn "vertices:"
    for v in g.Vertices do printfn "%A" v
    
    printfn "edges:"
    for e in g.Edges do printfn "%A" e


[<Test>]
let ``Empty undirected graph`` () = parseNames "graph { }" |> isDirected |> should be False


[<Test>]
let ``Empty directed graph`` () = parseNames "digraph { }" |> isDirected |> should be True


[<Test>]
let ``Named graph`` () = parseNames "graph t { }" |> isEmpty |> should be True


[<Test>]
let ``Single node`` () =
    let graph = parseNames "graph { a }"

    graph.VertexCount |> should equal 1
    graph.EdgeCount |> should equal 0

    graph |> shouldContainVertices [ "a" ]


[<Test>]
let ``Multiple nodes`` () =
    let graph = parseNames "graph { a b; c }"

    graph.VertexCount |> should equal 3
    graph.EdgeCount |> should equal 0

    graph |> shouldContainVertices [ "a"; "b"; "c" ]


[<Test>]
let ``Numeral node labels`` () =
    let graph = parseNames "graph { 1 2 }"

    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 0
    
    graph |> shouldContainVertices [ "1"; "2" ]


[<Test>]
let ``Single edge`` () =
    let graph = parseNames "graph { a -- b } "
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1
    
    graph |> shouldContainVertices [ "a"; "b" ]

[<Test>]
let ``Multiple edges`` () =
    let graph = parseNames "graph { a -- b c -- d }"

    graph.VertexCount |> should equal 4
    graph.EdgeCount |> should equal 2



[<Test>]
let ``Multiple edges in row`` () =
    let graph = parseNames "graph { a -- b c -- d -- e }"

    graph.VertexCount |> should equal 5
    graph.EdgeCount |> should equal 3


[<Test>]
let ``Multi-egde`` () =
    let graph = parseNames "graph { a -- b a -- b }"
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 2


[<Test>]
let ``Strict graph`` () =
    let graph = parseNames "strict graph { a -- b a -- b }"

    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1
    graph.AllowParallelEdges |> should be False
    // todo: add attributes


[<Test>]
let ``Keyword labels`` () =
    let graph = parseNames "graph { \"graph\" -- \"node\" }"
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1

    graph |> shouldContainVertices [ "graph"; "node" ]


[<Test>]
let ``Wrong edge in directed`` () =
    shouldFail (fun _ -> parseNames "graph { a -- b }" |> ignore)


[<Test>]
let ``Wrong edge in undirected`` () =
    shouldFail (fun _ -> parseNames "digraph { a -> b }" |> ignore)

[<Test>]
let ``Subgraph on left of edge`` () =
    let graph = parseNames "graph { { a b } -- c }"

    printGraph graph

    graph.VertexCount |> should equal 3
    graph.EdgeCount |> should equal 2

    graph |> shouldContainVertices [ "a"; "b"; "c" ]

[<Test>]
let ``Subgraph on right of edge`` () =
    let graph = parseNames "graph { a -- { b c } }"

    printGraph graph

    graph.VertexCount |> should equal 3
    graph.EdgeCount |> should equal 2

    graph |> shouldContainVertices [ "a"; "b"; "c" ]

[<Test>]
let ``Subgraph on both sides of edge`` () =
    let graph = parseNames "graph { { a b } -- { c d } }"

    printGraph graph

    graph.VertexCount |> should equal 4
    graph.EdgeCount |> should equal 4

    graph |> shouldContainVertices [ "a"; "b"; "c"; "d" ]


[<Test>]
let ``Nested subgraphs`` () =
    let graph = parseNames "graph { a -- { b -- { c d } } }"

    graph.VertexCount |> should equal 4
    graph.EdgeCount |> should equal 5

    graph |> shouldContainVertices [ "a"; "b"; "c"; "d" ]


[<Test>]
let ``Lot of nodes`` () =
    let graph = parseNames "graph { a b c; d \n e f }"

    graph |> shouldContainVertices [ "a"; "b"; "c"; "d"; "e"; "f"]


(* todo: add more sense to attr tests *)

[<Test>]
let ``Node attibute`` () =
    let graph = DotParser.parse nameWithAttr "graph { node [ color = red ] }"
    ()


[<Test>]
let ``Edge attibutes`` () =
    let graph = DotParser.parse nameWithAttr "graph { edge [ color = red, foo=bar; a = b ] }"
    ()


[<Test>]
let ``Empty attributes`` () =
    let graph = DotParser.parse nameWithAttr "graph { edge [ ] }"
    ()