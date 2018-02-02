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

let input1 = ">"
let input1 = "<>"
let input1 = "^>v<"
let input1 = "^v^v^v^v^v"

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day03.txt")
let input1 = File.ReadAllText file

input1
|> Seq.scan move (0,0)
|> Seq.fold updateCount Map.empty
|> Seq.length

// Part 2
let santaMoves (l : string) =
    let indexes = [0..2..l.Length-1]
    indexes
    |> Seq.map (fun n -> l.[n])
    |> List.ofSeq

let roboSantaMoves (l : string) =
    let indexes = [1..2..l.Length-1]
    indexes
    |> Seq.map (fun n -> l.[n])
    |> List.ofSeq

let input2 = "^v"
let input2 = "^v^v^v^v^v"
let input2 = File.ReadAllText file

input2 |> santaMoves
input2 |> roboSantaMoves

let presents presentTally input =
    input
    |> Seq.scan move (0,0)
    |> Seq.fold updateCount presentTally

let santaPresents presentTally input =
    input |> santaMoves |> presents presentTally

let roboSantaPresents presentTally input =
    input |> roboSantaMoves |> presents presentTally

let presentsFromSanta = input2 |> santaPresents (Map.empty.Add((0,0), 1))
input2 |> roboSantaPresents presentsFromSanta |> Seq.length
