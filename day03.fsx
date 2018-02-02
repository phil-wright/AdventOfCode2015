let move (x,y) direction =
    match direction with
    | '>' -> x+1,y
    | '<' -> x-1,y
    | '^' -> x,y-1
    | 'v' -> x,y+1
    | _ -> failwith "Invalid move"

let updateCount (tally : Map<(int * int), int>) house =
    match tally.TryFind house with
    | Some(numberOfPresents) ->
        let tempTally = tally.Remove house
        tempTally.Add (house, numberOfPresents+1)
    | None -> tally.Add (house, 1)

let input = ">"
let input = "<>"
let input = "^>v<"
let input = "^v^v^v^v^v"

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day03.txt")
let input = File.ReadAllText file

input
|> Seq.scan move (0,0)
|> Seq.fold updateCount Map.empty
|> Map.filter (fun _ n -> n > 1)
|> Seq.length
