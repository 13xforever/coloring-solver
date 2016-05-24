open System
open SolutionTypes
open Logic
open FieldsE

let testField: Field = array2D [
    [w; w; w]
    [w; b; w]
    [w; w; w]
]

let rng = new Random()
let testFieldRng: Field = Array2D.init 5 8 (fun nx ny -> match rng.NextDouble() with
                                                         | v when v < 0.80 -> w
                                                         | v when v < 0.90 -> y
                                                         | v when v < 0.95 -> r
                                                         | _ -> g )

[<EntryPoint>]
let main argv =
    let input = fieldE7
    let result = analyze input
    printfn "%A" result
    0 // return an integer exit code
