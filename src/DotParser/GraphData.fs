﻿module DotParserProject.GraphData

open Option
open System.Collections.Generic
open System.IO
open QuickGraph

[<AllowNullLiteral>]
type GraphData = class
    val private graph : IMutableVertexAndEdgeSet<string, SEdge<string>>
    val private edgesAttributes : Dictionary<string, string>
    val private graphAttributes : Dictionary<string, string>
    val private nodesAttributes : Dictionary<string, string>


    new (isDirected: bool, isStrict: bool) = {
        graph = if isDirected
                then new BidirectionalGraph<_,_> (isStrict) :> IMutableVertexAndEdgeSet<_,_>
                else new UndirectedGraph<_,_>    (isStrict) :> IMutableVertexAndEdgeSet<_,_>

        edgesAttributes = new Dictionary<_,_> ()
        graphAttributes = new Dictionary<_,_> ()
        nodesAttributes = new Dictionary<_,_> ()
    }

    member x.GetGraph () = x.graph

    member x.AddDefaultAttributes key attributes =
        let dict =
            match key with
            | "edge" -> Some x.edgesAttributes
            | "graph" -> Some x.graphAttributes
            | "node" -> Some x.nodesAttributes
            | _ -> None

        match dict with
        | Some d -> for (k, v) in attributes do d.[k] <- v
        | None -> ()

    member x.AddNode name =
        x.graph.AddVertex name |> ignore
        [name]

    member x.AddEdge d n2 =
        let g, n1 = d
        if List.length n1 <> List.length n2 then failwith "implement me" else ()
        
        List.iter2 (fun x1 x2 -> x.graph.AddEdge(new SEdge<_>(x1, x2)) |> ignore) n1 n2
        n2

    member private x.AddEdges nodes attributes =
//        match nodes with
//        | v1 :: v2 :: tail ->
//            x.edges.Add (v1, v2, attributes @ [for KeyValue(k, v) in x.edgesAttributes -> (k, v)])
//            x.AddEdges (v2 :: tail) attributes
//        | _ -> ()
        printf "add edges"


    member private x.CreateNode node =
//        if not <| x.nodes.ContainsKey node then
//            let dict = new Dictionary<_, _>()
//            for KeyValue(k, v) in x.nodesAttributes do dict.[k] <- v
//            x.nodes.[node] <- dict
            printf "create node"

//            x.graph.AddVertex(node, dict) |> ignore


    member x.AddEdgeStmtData nodes attributes = 
        x.AddEdges nodes attributes
        for node in nodes do x.CreateNode node


    member x.AddNodeStmtData node attributes =
        x.CreateNode node
        
//        let dict = x.nodes.[node]
//        for k, v in attributes do
//             dict.[k] <- v


//    member x.AddAssignStmt key value =
//        x.assign_stmt_list.Add (key, value) |> ignore 
       
    // todo: remove or move to Utils    
    member x.PrintAllCollectedData() =
        printfn "\nNodes attributes:"
        for entry in x.nodesAttributes do printfn "%A: %A" entry.Key entry.Value

        printfn "\nEdges attributes:"
        for entry in x.edgesAttributes do printfn "%A: %A" entry.Key entry.Value

//        printfn "\nEdges:"
//        for edge in x.edges do printfn "%A" edge

//        printfn "\nNodes:"
//        for KeyValue(k, v) in x.nodes do printfn "%A: %A" k v
end