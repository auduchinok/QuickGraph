module DotParserProject.GraphData

open Option
open System.Collections.Generic
open System.IO
open QuickGraph

type Attr = string * string
type Node = string * Attr list

type GraphData (isDirected: bool, isStrict: bool, edgeAttributes, graphAttributes, nodeAttributes) =
    let graph = if isDirected
                then new BidirectionalGraph<_,_> (not isStrict) :> IMutableVertexAndEdgeSet<_,_>
                else new UndirectedGraph<_,_>    (not isStrict) :> IMutableVertexAndEdgeSet<_,_>

    new (isDirected: bool, isStrict: bool) = 
        GraphData(isDirected,
                  isStrict,
                  Dictionary<string, string>(),
                  Dictionary<string, string>(),
                  Dictionary<string, string>())

    new (g: GraphData) =
        GraphData(g.IsDirected,
                  g.IsStrict,
                  Dictionary<_,_> (g.EdgeAttributes :> Dictionary<_,_>),
                  Dictionary<_,_> (g.GraphAttributes :> Dictionary<_,_>),
                  Dictionary<_,_> (g.NodeAttributes :> Dictionary<_,_>)) 
    

    member x.Graph with get() = graph
    member x.IsDirected with get() = isDirected
    member x.IsStrict   with get() = isStrict
    member x.EdgeAttributes  with get() = edgeAttributes
    member x.GraphAttributes with get() = graphAttributes
    member x.NodeAttributes  with get() = nodeAttributes


    member x.AddAttributes key (attr: Attr list) =
        let dict =
            match key with
            | "graph" -> graphAttributes
            | "edge"  -> edgeAttributes
            | "node"  -> nodeAttributes
            | _ -> failwith "Wrong attribute key"

        for (k, v) in attr do dict.[k] <- v


    member x.AddNode name : Node list =
        graph.AddVertex name |> ignore
        [name, []]


    member x.AddEdge (ns1: Node list) (ns2: Node list) =
        for (n1, _) in ns1 do
            for (n2, _) in ns2 do
                graph.AddEdge(new STaggedEdge<_,_>(n1, n2, [])) |> ignore
        ns2


    member x.AddSubgraph (g: GraphData) : Node list = (* copy attributes *)
        for v in g.Graph.Vertices do x.AddNode v |> ignore
        for e in g.Graph.Edges do
            let n1, n2, attr = e.Source, e.Target, e.Tag
            x.AddEdge [(n1, [])] [(n2, [])] |> ignore
        g.Graph.Vertices |> List.ofSeq |> List.map (fun x -> (x, []))