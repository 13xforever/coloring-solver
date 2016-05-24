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
    let partBuild = buildIsland field map
    [ for i in islandCoords -> partBuild i ]

let analyze (field: Field): FieldInfo =
    let width = field.GetLength 0
    let height = field.GetLength 1
    let map = Array2D.zeroCreate<int> width height
    let map, islandCount, islandCoords = mapIslands field [] map 1
    let islands = buildIslandLinks field map islandCount (List.rev islandCoords)
    { field = field; map = map; islandCount = islandCount; islands = islands }
