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