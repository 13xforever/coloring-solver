module SolutionTypes

type Color = char
type Field = Color[,]
type IslandMap = int[,]
type Coords = {
    x: int;
    y: int
}
type Island = {
    id: int;
    color: Color;
    coords: Coords;
    neighbours: list<Island>
}
type FieldInfo = {
    field: Field;
    islandCount: int;
    islands: list<Island>
}
type SolutionStep = {
    input: FieldInfo
    change: Island
    output: FieldInfo
}
type Solution = list<SolutionStep>

