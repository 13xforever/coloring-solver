﻿module SolutionTypes

type Color = char
type Field = Color[,]
type IslandMap = int[,]
type Coords = {
    x: int
    y: int
}
type Island = {
    id: int
    color: Color
    coords: Coords
    neighbours: Set<int>
}
type FieldInfo = {
    colorsCount: int
    islands: list<Island>
}
type SolutionStep = {
    input: FieldInfo
    change: Island * Color
    output: FieldInfo
}
type Solution = list<SolutionStep>

let w: Color = 'w'
let y: Color = 'y'
let r: Color = 'r'
let c: Color = 'c'
let b: Color = 'b'

let validColors = [| w, y, r, c, b |]

let getWidth (array: 'T[,]): int = array.GetLength 0
let getHeight (array: 'T[,]): int = array.GetLength 1
