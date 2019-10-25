module Hamming

let hammingCount (strand1: string) (strand2: string) =
  let strand1List = strand1 |> Seq.toList
  let strand2List = strand2 |> Seq.toList
  let combinedStrand = Seq.zip strand1List strand2List |> Seq.toList
  let hammingList = combinedStrand |> List.filter (fun (x,y) -> x<>y)
  hammingList |> List.length

let distance (strand1: string) (strand2: string): int option =
  match strand1.Length = strand2.Length with
  | true -> Some (hammingCount strand1 strand2)
  | false -> None