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

let recolorNeighbours (island: Island) newColor (fieldInfo: FieldInfo) (islandMap: Map<int, Island>) =
    let neighboursToMerge = island.neighbours
                            |> Seq.where (fun i -> islandMap.[i].color = newColor)
                            |> Set.ofSeq
    let neighboursOfNeighbours = seq { for id in neighboursToMerge do yield! islandMap.[id].neighbours }
                                 |> Seq.where (fun n -> n <> island.id )
                                 |> Set.ofSeq
    let updatedNeighboursOfNeighbours = seq { for i in fieldInfo.islands do
                                                if (neighboursOfNeighbours.Contains i.id) && i.id <> island.id then
                                                        let newNeighbours = (i.neighbours - neighboursToMerge).Add island.id
                                                        yield { i with neighbours = newNeighbours } }
                                        |> Seq.toList
    let updatedIsland = { island with color = newColor; neighbours = island.neighbours - neighboursToMerge + neighboursOfNeighbours }
    let newIslands = fieldInfo.islands
                        |> List.where (fun i -> not ((neighboursToMerge.Contains i.id) || (neighboursOfNeighbours.Contains i.id) || (i.id = island.id)))
                        |> List.append (updatedIsland::updatedNeighboursOfNeighbours)
    let colorsCount = countUniqueColors newIslands
    { fieldInfo with colorsCount = colorsCount; islands = newIslands }

let possibleChanges fieldInfo =
    if fieldInfo.islands.Length = 1 then
        Seq.empty
    else
        let sortedIslands = fieldInfo.islands
                            |> Seq.sortByDescending (fun i -> i.neighbours.Count)
        seq { for island in sortedIslands do
                let islandMap = fieldInfo.islands |> Seq.map (fun i -> i.id, i) |> Map.ofSeq
                let neighbourColors = seq { for neighbour in island.neighbours do
                                                yield islandMap.[neighbour].color }
                                      |> Seq.distinct
                for newColor in neighbourColors do
                    let newFieldInfo = recolorNeighbours island newColor fieldInfo islandMap
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
    printfn "Islands to consider: %A" start.islands.Length
    let solutions = findSolutions start maxLength []
    if maxLength < 1 then
        let result = solutions |> Seq.minBy (fun s -> s.Length)
        Some(result)
    else
        solutions
        |> Seq.skipWhile (fun s -> s.Length > maxLength)
        |> Seq.tryHead
