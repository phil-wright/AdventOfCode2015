//
// (()) and ()() both result in floor 0.
// ((( and (()(()( both result in floor 3.
// ))((((( also results in floor 3.
// ()) and ))( both result in floor -1 (the first basement level).
// ))) and )())()) both result in floor -3
//

let changeFloor floor direction =
    match direction with
    | '(' -> floor + 1
    | _ -> floor - 1

let santaPart1 input =
    input |> Seq.fold changeFloor 0

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
    |> Seq.scan changeFloor 0
    |> Seq.findIndex (fun n -> n = -1)

santaPart2 ")"
santaPart2 "()())"

santaPart2 santaMovement