module Logic

open SolutionTypes

let getWidth (array: 'T[,]): int = array.GetLength 0
let getHeight (array: 'T[,]): int = array.GetLength 1

let tryFindIndex (map: 'T[,]) predicate =
    let width = getWidth map
    let height = getHeight map
    seq { for x in 0..width-1 do
            for y in 0..height-1 do
                yield (x, y, map.[x, y]) }
    |> Seq.skipWhile predicate
    |> Seq.tryHead

let rec fillNewIsland (field: Field) color id x y (map: IslandMap)=
    if (x < 0 || x >= getWidth field || y < 0 || y >= getHeight field || map.[x, y] <> 0 || field.[x, y] <> color) then
        map
    else
        map.[x, y] <- id
        let partFill = fillNewIsland field color id
        map |> partFill (x - 1) y
            |> partFill (x + 1) y
            |> partFill x (y - 1)
            |> partFill x (y + 1)

let findNewIsland field map id =
    let coords = tryFindIndex map (fun (x, y, v) -> v <> 0)
    if (coords.IsSome) then
        let x, y, _ = coords.Value
        Some(x, y, fillNewIsland field field.[x,y] id x y map)
    else
        None

let rec mapIslands field islandCoords map id: IslandMap * int * list<int * int * int> =
    match findNewIsland field map id with
    | Some(x, y, result) -> mapIslands field ((id, x, y)::islandCoords) result (id + 1)
    | None -> (map, id - 1, islandCoords)

let rec findNeigbours map id x y (neighbours: Set<int>) =
    if (x < 0 || x >= getWidth map || y < 0 || y >= getHeight map || map.[x, y] = 0) then
        neighbours
    else
        match map.[x, y] with
        | n when n <> id -> neighbours.Add n
        | _ -> map.[x, y] <- 0
               let partFind = findNeigbours map id
               neighbours |> partFind (x - 1) y
                          |> partFind (x + 1) y
                          |> partFind x (y - 1)
                          |> partFind x (y + 1)

let buildIsland (field: Field) map islandCoords: Island =
    let id, x, y = islandCoords
    { id = id;
      color = field.[x, y];
      coords = {x = x; y = y};
      neighbours = findNeigbours (Array2D.copy map) id x y Set.empty; }

let buildIslandLinks field map islandCount islandCoords: list<Island> =
    [ for coords in islandCoords -> buildIsland field map coords ]

let countColors (field: Field) islandCoords =
    let uniqueColors = seq { for id, x, y in islandCoords do yield field.[x, y] } |> Set.ofSeq
    uniqueColors.Count

let analyze field: FieldInfo =
    let width = getWidth field
    let height = getHeight field
    let map = Array2D.zeroCreate<int> width height
    let map, islandCount, islandCoords = mapIslands field [] map 1
    let islands = buildIslandLinks field map islandCount (List.rev islandCoords)
    let colors = countColors field islandCoords
    { field = field; map = map; islandCount = islandCount; colorsCount = colors; islands = islands }

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
                for neighbour in island.neighbours do
                    let newColor = allIslands.[neighbour].color
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
                        let i, _ = c.change
                        printfn "Evaluating island %A" i.id
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