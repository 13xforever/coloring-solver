open SolutionTypes

let w = 'w'
let y = 'y'
let r = 'r'
let g = 'g'
let b = 'b'

let testField: Field = array2D [
    [w; w; w]
    [w; b; w]
    [w; w; w]
]

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
