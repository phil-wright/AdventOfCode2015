//
// A present with dimensions of 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
// A present with dimensions of 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.
//


let split (by : char) (input : string) =
        input.Split(by)

let paperNeeded (input : string) =
    let parseDimensions (input : string) (splitBy : char) =
        input |> split splitBy |> Array.map int

    let [| length; width; height |] = parseDimensions input 'x'
    let area1 = length * width
    let area2 = width * height
    let area3 = length * height
    let area = 2*area1 + 2*area2 + 2*area3
    let extraPaper = List.min [ area1; area2; area3 ]
    area + extraPaper

paperNeeded "2x3x4"
paperNeeded "1x1x10"
paperNeeded "20x3x11"
paperNeeded "15x27x5"
paperNeeded "6x29x7"
paperNeeded "30x15x9"
paperNeeded "10x4x15"

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day02.txt")
File.ReadAllLines(file)
|> Seq.sumBy paperNeeded

let perimeter a b =
    2*a + 2*b

let ribbonNeeded  (input : string) =
    let parseDimensions (input : string) (splitBy : char) =
        input |> split splitBy |> Array.map int

    let [| length; width; height |] = parseDimensions input 'x'
    let p1 = perimeter length width
    let p2 = perimeter width height
    let p3 = perimeter length height
    let perimeter = List.min [ p1; p2; p3 ]
    let bowRibbon = length * width * height
    perimeter + bowRibbon

ribbonNeeded "2x3x4"
ribbonNeeded "1x1x10"

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day02.txt")
File.ReadAllLines(file)
|> Seq.sumBy ribbonNeeded