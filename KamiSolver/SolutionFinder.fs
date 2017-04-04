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

let possibleChanges fieldInfo =
    if fieldInfo.islandCount = 1 then
        Seq.empty
    else
        let allIslands = [ for i in fieldInfo.islands -> i.id, i] |> Map.ofList
        seq {
            for island in fieldInfo.islands do
                let neighbourColors = Set.ofSeq (seq { for neighbour in island.neighbours do yield allIslands.[neighbour].color })
                for newColor in neighbourColors do
                    let newField = recolor island.color newColor island.coords.x island.coords.y (Array2D.copy fieldInfo.field)
                    let newFieldInfo = analyze newField
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
    printfn "Islands to consider: %A" start.islandCount
    let solutions = findSolutions start maxLength []
    if maxLength = 0 then
        let result = solutions |> Seq.minBy (fun s -> s.Length)
        Some(result)
    else
        solutions
        |> Seq.skipWhile (fun s -> s.Length > maxLength)
        |> Seq.tryHead
