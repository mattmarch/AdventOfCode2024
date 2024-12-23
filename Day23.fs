module AdventOfCode2024.Day23

open AdventOfCode2024.Common

let parseLine = splitBy "-" >> unpack2

let buildNetworkMap connections =
    let connectionsIncludingRev =
        connections |> Seq.map (fun (a, b) -> b, a) |> Seq.append connections

    connectionsIncludingRev
    |> Seq.groupBy fst
    |> Seq.map (fun (node, group) -> node, group |> Seq.map snd |> Set.ofSeq)
    |> Map.ofSeq

let findGroupsOf3WithNode networkMap node =
    let connectedNodes = networkMap |> Map.find node

    connectedNodes
    |> Seq.map (fun n ->
        let sharedConnections = Map.find n networkMap |> Set.intersect connectedNodes
        sharedConnections |> Set.map (fun conn -> [ node; n; conn ] |> set))
    |> Set.unionMany

let getNodesBeginningWithT networkMap =
    networkMap |> Map.keys |> Seq.filter (fun (n: string) -> n.StartsWith("t"))

let solve () =
    let networkMap = readLines "23" |> Seq.map parseLine |> buildNetworkMap

    let result =
        getNodesBeginningWithT networkMap
        |> Seq.map (findGroupsOf3WithNode networkMap)
        |> Set.unionMany
        |> Set.count

    printfn $"Day 23 - Part 1: %d{result}"
