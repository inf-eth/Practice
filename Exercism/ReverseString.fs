module ReverseString

let reversefun (input: string) =
  let inputList = input |> Seq.toList
  let rec reverseList acc inputacc =
    match inputacc with
    | [] -> acc
    | hd::tl -> reverseList (hd::acc) tl
  let reversedInputList = reverseList [] inputList
  let reversedInput = reversedInputList |> Seq.toArray |> System.String
  reversedInput

let reverse (input: string): string = //input |> Seq.toList |> List.rev |> Seq.toArray |> System.String
  reversefun input