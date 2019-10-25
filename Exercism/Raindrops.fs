module Raindrops

// [3;5;7] |> checkmultiples  gives [3;7]
// [3;7] |> translate gives [Pling;Plang]

let factorise (num:int): (int List) =
  let testNums = [1..num]
  testNums |> List.filter (fun x -> num%x=0)

let translate (num:int): string =
  match num with
  | 3 -> "Pling"
  | 5 -> "Plang"
  | 7 -> "Plong"
  | _ -> ""

// let lst = [1..10]
// let folded = List.fold (fun x y -> x+y) 0 lst
let concatenateStrings (st1:string) (st2:string): string = st1+st2

let convert (number: int): string = 
  let factors = factorise number
  let mappedFactors = factors |> List.map translate |> List.fold (fun st curr -> (concatenateStrings st curr) ) ""
  match mappedFactors with
  | "" -> string number
  | _ -> mappedFactors
