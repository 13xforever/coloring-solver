open System
open SolutionTypes
open Logic

let w: Color = 'w'
let y: Color = 'y'
let r: Color = 'r'
let g: Color = 'g'
let b: Color = 'b'

let validColors = [| w, y, r, g, b |]

let testField: Field = array2D [
    [w; w; w]
    [w; b; w]
    [w; w; w]
]

let rng = new Random()
let testField2: Field = Array2D.init 5 8 (fun nx ny -> match rng.NextDouble() with
                                                         | v when v < 0.80 -> w
                                                         | v when v < 0.90 -> y
                                                         | v when v < 0.95 -> r
                                                         | _ -> g )

[<EntryPoint>]
let main argv =
    let input = testField2
    printfn "Input:"
    printfn "%A" input
    let result = analyze input
    printfn "Islands (%A):" result.islandCount
    printfn "%A" result.map
    0 // return an integer exit code
