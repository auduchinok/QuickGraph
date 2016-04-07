module DotParserProject.Utils

open Option
open DotParserProject.GraphData
open System

let log msgList = 
    msgList |> List.map (printfn "Log: %s") |> ignore
    printfn ""

let optToStr (optValue: string option) =
    match optValue with
    | Some value -> value
    | None -> ""

let type_key = "type"
let strict_key = "is_strict"
let name_key = "name"

let addSubgraphToArray subgr_id (gr: GraphData) (array: ResizeArray<GraphData>) =
//    if (isNone subgr_id || isNone (get subgr_id)) then 
//        gr.AddGeneralInfo [strict_key, ""; type_key, "subgraph"; name_key, System.Guid.NewGuid().ToString()]
//    else
//        gr.AddGeneralInfo [strict_key, ""; type_key, "subgraph"; name_key, get (get subgr_id)]
    array.Add gr
//    gr.GetName()
    "Yo. Implement Utils"