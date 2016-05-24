module Logic

open SolutionTypes

let tryFindIndex (map: IslandMap) width height =
    seq { for x in 0..width-1 do
            for y in 0..height-1 do
                yield (x, y, map.[x, y]) }
    |> Seq.skipWhile (fun (x, y, v) -> v <> 0)
    |> Seq.tryHead

let rec fillNewIsland (field: Field) color width height id x y (map: IslandMap)=
    if (x < 0 || x >= width || y < 0 || y >= height || map.[x, y] <> 0 || field.[x, y] <> color) then
        map
    else
        map.[x, y] <- id
        let partFill = fillNewIsland field color width height id
        map |> partFill (x-1) y
            |> partFill (x+1) y
            |> partFill x (y-1)
            |> partFill x (y+1)

let findNewIsland field map width height id =
    let coords = tryFindIndex map width height
    if (coords.IsSome) then
        let x, y, _ = coords.Value
        Some(fillNewIsland field field.[x,y] width height id x y map)
    else
        None

let rec findIslandIds field width height map id: IslandMap =
    match findNewIsland field map width height id with
    | Some(result) -> findIslandIds field width height result (id + 1)
    | None -> map

let analyze (field: Field): FieldInfo =
    let width = field.GetLength 0
    let height = field.GetLength 1
    let map = Array2D.zeroCreate<int> width height
    let map = findIslandIds field width height map 1

    { field = field; map = map; islandCount = -1; islands = [] }
