//
// (()) and ()() both result in floor 0.
// ((( and (()(()( both result in floor 3.
// ))((((( also results in floor 3.
// ()) and ))( both result in floor -1 (the first basement level).
// ))) and )())()) both result in floor -3
//

let changeFloor direction =
    match direction with
    | '(' -> +1
    | ')' -> -1
    | _   -> failwith "invalid character"

let santaPart1 input =
    input |> Seq.sumBy changeFloor

santaPart1 "(())"
santaPart1 "()()"

santaPart1 "((("
santaPart1 "(()(()("
santaPart1 "))((((("

santaPart1 "())"
santaPart1 "))("

santaPart1 ")))"
santaPart1 ")())())"

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day01.txt")
let santaMovement = File.ReadAllText file

santaPart1 santaMovement

// ) causes him to enter the basement at character position 1.
// ()()) causes him to enter the basement at character position 5.

let santaPart2 s =
    s
    |> Seq.map changeFloor
    |> Seq.scan (+) 0
    |> Seq.findIndex (fun n -> n = -1)

santaPart2 ")"
santaPart2 "()())"

santaPart2 santaMovement