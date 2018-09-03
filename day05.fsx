(*
A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

For example:

ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.
*)

let s = "aei"
let s = "xazegov"
let s = "aeiouaeiouaeiou"

let isVowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

let hasthreeOrMoreVowels (input : string) =
    let numberOfVowels = 
        input
        |> Seq.where isVowel
        |> Seq.length
    numberOfVowels >= 3

hasthreeOrMoreVowels s

let s = "xx"
let s = "abcdde"
let s = "aabbccdd"

s |> Seq.pairwise

let isLetterTwiceInARow (input : string) =
    input
    |> Seq.pairwise
    |> Seq.exists (fun (a,b) -> a = b)

let naughty (input : string) =
    input.Contains("ab") || input.Contains("cd") || input.Contains("pq") || input.Contains("xy")

let s = "abc|"
let s = "acb"
naughty s

let s = "ugknbfddgicrmopn" // is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
let s = "aaa" // is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
let s = "jchzalrnumimnmhp" // is naughty because it has no double letter.
let s = "haegwjzuvuyypxyu" // is naughty because it contains the string xy.
let s = "dvszwmarrgswjxmb" // is naughty because it contains only one vowel.

let isNice s = hasthreeOrMoreVowels s && isLetterTwiceInARow s && not (naughty s)

open System.IO
let file = Path.Combine(__SOURCE_DIRECTORY__, "day05.txt")
let inputLines = File.ReadAllLines(file)

inputLines
|> Seq.filter isNice
|> Seq.length

// Part 2
//for each position
//  get the next two characters
//  check if they're equal to anything at position+2 onwards - use position+2 to avoid overlap
let s = "aaa"
let s = "xyxy"
let s = "aabcdefgaa"
s.[1..2]
s |> Seq.windowed 2

// repeated character with one letter between them
let p = ["a";"a";"a"] 
let p = ["a";"a"]

match p with
| [a;b;c] when a = c -> true
| _::xs -> false
| [] -> false

