﻿module SolutionTypes

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
    neighbours: Set<int>
}
type FieldInfo = {
    field: Field;
    map: IslandMap;
    islandCount: int;
    islands: list<Island>
}
type SolutionStep = {
    input: FieldInfo
    change: Island
    output: FieldInfo
}
type Solution = list<SolutionStep>

let w: Color = 'w'
let y: Color = 'y'
let r: Color = 'r'
let g: Color = 'g'
let b: Color = 'b'

let validColors = [| w, y, r, g, b |]
