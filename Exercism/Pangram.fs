module Pangram

let addChar (charMap: Map<char, int>) (ch: char) =
  charMap.Add(ch,1)//charMap.Add(ch,((charMap.[ch])+1))

let alphabets = "abcdefghijklmnopqrstuvwxyz"
let alphabetList = alphabets |> (List.ofSeq)
let findandDelete (ch:char) (lst:char List): (char List) = lst |> List.filter (fun x -> x<>ch)
let test = alphabetList |> (findandDelete 'a')
let isEmptyList lst = List.isEmpty lst
let test1 ="\"Five quacking Zephyrs jolt my wax bed.\""
let another = test1.ToLower()
let go = "=================================="

let isPangram (input: string): bool = 
  let tempInput = input.ToLower()
  let filterInput inputString alphabetList =
    let inputCharList = inputString |> (List.ofSeq)
    let rec recFilterInput accChar accAlphabetList =
      printfn "%A" accChar
      printfn "%A" accAlphabetList
      match accChar,accAlphabetList with
      | [],[] -> true
      | hd::tl,[] -> true
      | [],hd::tl -> false
      | hd::tl,aLst -> recFilterInput (tl) (findandDelete hd aLst)
    recFilterInput inputCharList alphabetList
  filterInput tempInput alphabetList
