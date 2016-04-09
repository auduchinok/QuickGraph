module DotParser.Tests.Tests

open FsUnit
open NUnit.Framework
open DotParser
open System.Collections
open System.Linq

let isDirected (g: QuickGraph.IMutableVertexAndEdgeSet<_,_>) = g.IsDirected
let isStrict (g: QuickGraph.IMutableVertexAndEdgeSet<_,_>) = not g.AllowParallelEdges
let isEmpty (g: QuickGraph.IMutableVertexAndEdgeSet<_,_>) = g.IsVerticesEmpty

let shouldContainVertices names (g: QuickGraph.IMutableVertexAndEdgeSet<_,_>) =
    List.forall (fun n -> g.Vertices.Contains n) names |> should be True


[<Test>]
let ``Empty undirected graph`` () = DotParser.parse "graph { }" |> isDirected |> should be False


[<Test>]
let ``Empty directed graph`` () = DotParser.parse "digraph { }" |> isDirected |> should be True


[<Test>]
let ``Named graph`` () = DotParser.parse "graph t { }" |> isEmpty |> should be True


[<Test>]
let ``Single node`` () =
    let graph = DotParser.parse "graph { a }"

    graph.VertexCount |> should equal 1
    graph.EdgeCount |> should equal 0

    graph |> shouldContainVertices [ "a" ]


[<Test>]
let ``Multiple nodes`` () =
    let graph = DotParser.parse "graph { a b; c }"

    graph.VertexCount |> should equal 3
    graph.EdgeCount |> should equal 0

    graph |> shouldContainVertices [ "a"; "b"; "c" ]


[<Test>]
let ``Numeral node labels`` () =
    let graph = DotParser.parse "graph { 1 2 }"

    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 0
    
    graph |> shouldContainVertices [ "1"; "2" ]


[<Test>]
let ``Single edge`` () =
    let graph = DotParser.parse "graph { a -- b } "
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1
    
    graph |> shouldContainVertices [ "a"; "b" ]

[<Test>]
let ``Multiple edges`` () =
    let graph = DotParser.parse "graph { a -- b c -- d }"

    graph.VertexCount |> should equal 4
    graph.EdgeCount |> should equal 2


[<Test>]
let ``Multiple edges in row`` () =
    let graph = DotParser.parse "graph { a -- b c -- d -- e }"

    graph.VertexCount |> should equal 5
    graph.EdgeCount |> should equal 3


[<Test>]
let ``Multi-egde`` () =
    let graph = DotParser.parse "graph { a -- b a -- b }"
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 2


[<Test>]
let ``Strict graph`` () =
    let graph = DotParser.parse "strict graph { a -- b a -- b }"

    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1
    // todo: add attributes


[<Test>]
let ``Keyword labels`` () =
    let graph = DotParser.parse "graph { \"graph\" -- \"node\" }"
    
    graph.VertexCount |> should equal 2
    graph.EdgeCount |> should equal 1

    graph |> shouldContainVertices [ "graph"; "node" ]


[<Test>]
let ``Wrong edge in directed`` () =
    shouldFail (fun _ -> DotParser.parse "graph { a -> b }" |> ignore)


[<Test>]
let ``Wrong edge in undirected`` () =
    shouldFail (fun _ -> DotParser.parse "digraph { a - b }" |> ignore)

