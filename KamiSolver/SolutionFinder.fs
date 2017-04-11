module SolutionFinder

open SolutionTypes
open FieldAnalyzer

let rec recolor oldColor newColor x y (field: Field) =
    if (x < 0 || x >= getWidth field || y < 0 || y >= getHeight field || field.[x, y] = newColor || field.[x, y] <> oldColor) then
        field
    else
        field.[x, y] <- newColor
        let partRecolor = recolor oldColor newColor
        field |> partRecolor (x - 1) y
              |> partRecolor (x + 1) y
              |> partRecolor x (y - 1)
              |> partRecolor x (y + 1)

let printIslands islands =
    let listToPrint = islands
                        |> Map.toSeq
                        |> Seq.map (fun (_, i) -> i.id, i.color, i.neighbours)
                        |> Seq.sortBy (fun (id, _, _) -> id)
                        |> List.ofSeq
    printfn "%A" listToPrint

let recolorNeighbours (island: Island) newColor (fieldInfo: FieldInfo) =
 //   printIslands fieldInfo.islands
//    printfn "%A ==> %A" island.color newColor

    let neighboursToMerge = island.neighbours
                            |> Seq.where (fun i -> fieldInfo.islands.[i].color = newColor)
                            |> Set.ofSeq

//    printfn "neighboursToMerge: %A" (neighboursToMerge |> List.ofSeq)

    let neighboursOfNeighbours = seq { for id in neighboursToMerge do yield! fieldInfo.islands.[id].neighbours }
                                 |> Seq.distinct
                                 |> Seq.where (fun n -> n <> island.id )
                                 |> Set.ofSeq
    let updatedNeighboursOfNeighbours = seq { for i in (fieldInfo.islands |> Map.toSeq |> Seq.map (fun (_, i) -> i)) do
                                                if (neighboursOfNeighbours.Contains i.id) && i.id <> island.id then
                                                        let newNeighbours = (i.neighbours - neighboursToMerge).Add island.id
                                                        yield { i with neighbours = newNeighbours } }
                                        |> List.ofSeq

//    printfn "updatedNeighboursOfNeighbours: %A" (updatedNeighboursOfNeighbours |> Seq.map (fun i -> i.id, i.neighbours) |> List.ofSeq)

    let updatedIslandNeighbours = island.neighbours - neighboursToMerge + neighboursOfNeighbours

//    printfn "updatedIsland Neighbours: %A" updatedIslandNeighbours

    let updatedIsland = { island with color = newColor; neighbours = updatedIslandNeighbours }
    let newIslands = fieldInfo.islands
                        |> Map.toSeq
                        |> Seq.map (fun (_, i) -> i)
                        |> Seq.where (fun i -> not ((neighboursToMerge.Contains i.id) || (neighboursOfNeighbours.Contains i.id) || (i.id = island.id)))
                        |> Seq.append updatedNeighboursOfNeighbours
                        |> Seq.append (Seq.singleton updatedIsland)
                        |> Seq.map (fun i -> i.id, i)
                        |> Map.ofSeq

//    printIslands newIslands
//    printfn "\n"

    let colorsCount = countUniqueColors newIslands
    { fieldInfo with colorsCount = colorsCount; islands = newIslands }

let possibleChanges fieldInfo =
    if fieldInfo.islands.Count = 1 then
        Seq.empty
    else
        let sortedIslands = fieldInfo.islands
                            |> Map.toSeq
                            |> Seq.map (fun (_, i) -> i)
                            |> Seq.sortByDescending (fun i -> i.neighbours.Count)
                            |> List.ofSeq
//        printfn "sortedIslands: %A" (sortedIslands |> List.map (fun i -> i.id, i.neighbours))
//        printfn "Islands before recoloring:"
//        printIslands fieldInfo.islands
        seq {
            for island in sortedIslands do
//                printfn "Looking for neighbourColors of island %A: %A" island.id (island.neighbours)
                let neighbourColors = seq { for neighbour in island.neighbours do
                                                yield fieldInfo.islands.[neighbour].color }
                                      |> Seq.distinct
                                      |> List.ofSeq
//                printfn "neighbourColors: %A" neighbourColors
                for newColor in neighbourColors do
//                    printfn "recoloring %A from %A to %A" island.id island.color newColor
                    let newFieldInfo = recolorNeighbours island newColor fieldInfo
                    yield { input = fieldInfo;
                            change = (island, newColor);
                            output = newFieldInfo } }

let rec findSolutions (fieldInfo: FieldInfo) maxLength (solution: Solution): seq<Solution> =
    if maxLength > 0 && ((maxLength - solution.Length < fieldInfo.colorsCount - 1)  || solution.Length > maxLength) then
        Seq.empty
    else        
        let changes = possibleChanges fieldInfo
        if Seq.isEmpty changes then
            Seq.singleton solution
        else
            seq { for c in changes do
                    if solution.Length = 0 then
                        let i, nc = c.change
                        printfn "Evaluating island %A (%A -> %A)" i.id i.color nc
                    else
                        ()
                    yield! findSolutions c.output maxLength (c::solution) }

let solve field maxLength =
    let start = analyze field
    printfn "Islands to consider: %A" start.islands.Count
    //printfn "start: %A" start
    let solutions = findSolutions start maxLength []
    if maxLength = 0 then
        let result = solutions |> Seq.minBy (fun s -> s.Length)
        Some(result)
    else
        solutions
        |> Seq.skipWhile (fun s -> s.Length > maxLength)
        |> Seq.tryHead
