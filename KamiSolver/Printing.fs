module Printing

open System
open SolutionTypes

let printIslands (islands: seq<Island>) =
    let listToPrint = islands
                        |> Seq.map (fun i -> i.id, i.color, i.neighbours)
                        |> Seq.sortBy (fun (id, _, _) -> id)
                        |> List.ofSeq
    printfn "%A" listToPrint

let printField (field: Field) pointX pointY =
    let oldFgColor = Console.ForegroundColor
    let oldBgColor = Console.BackgroundColor
    for Y in 0..(getHeight field)-1 do
        for X in 0..(getWidth field)-1 do
            match field.[X, Y] with
            | color when color=w -> Console.BackgroundColor <- ConsoleColor.White
                                    Console.ForegroundColor <- ConsoleColor.Black
            | color when color=y -> Console.BackgroundColor <- ConsoleColor.DarkYellow
                                    Console.ForegroundColor <- ConsoleColor.Black
            | color when color=r -> Console.BackgroundColor <- ConsoleColor.DarkRed
                                    Console.ForegroundColor <- ConsoleColor.White
            | color when color=c -> Console.BackgroundColor <- ConsoleColor.DarkCyan
                                    Console.ForegroundColor <- ConsoleColor.Black
            | color when color=b -> Console.BackgroundColor <- ConsoleColor.Black
                                    Console.ForegroundColor <- ConsoleColor.White
            | color -> raise (InvalidOperationException(sprintf "Unknown color %A" color))
            if (X, Y) = (pointX, pointY) then
                Console.Write "X"
            else
                Console.Write "."
        Console.WriteLine ""
    Console.ForegroundColor <- oldFgColor
    Console.BackgroundColor <- oldBgColor
    ()