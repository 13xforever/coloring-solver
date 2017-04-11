open System
open SolutionTypes
open SolutionFinder
open FieldsB
open FieldsE

let testField: Field = array2D [ [w; w; w]
                                 [w; b; w]
                                 [w; w; b] ]

let testField2: Field = array2D [ [b; w; b]
                                  [r; b; w]
                                  [b; w; b] ]

let rng = new Random()
let testFieldRng: Field = Array2D.init 14 9 (fun nx ny -> match rng.NextDouble() with
                                                           | v when v < 0.80 -> w
                                                           | v when v < 0.90 -> y
                                                           | v when v < 0.95 -> r
                                                           | _ -> c )

[<EntryPoint>]
let main argv =
    //let input = fieldE7
    let input = testField2
    //let input = testFieldRng
    printfn "Input:"
    printfn "%A" input
    printfn ""

    let maxSteps = 0
    if maxSteps < 1 then
        printfn "Looking for optimal solution..."
    else
        printfn "Looking for first solution in %A steps..." maxSteps
    printfn ""
    let solution = solve input maxSteps
    if solution.IsNone then
        printfn "No solution in %A step(s) or fewer" maxSteps
    else
        printfn "Solution in %A step(s):" solution.Value.Length
        let changedField = input
        for s in List.rev solution.Value do
            let i, c = s.change
            let changedField = recolor i.color c i.coords.x i.coords.y changedField
            printfn ""
            printfn "Change %A to %A at (%A, %A) to get" i.color c i.coords.x i.coords.y
            printfn "%A" changedField
    0 // return an integer exit code
