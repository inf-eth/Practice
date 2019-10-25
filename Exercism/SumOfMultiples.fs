module SumOfMultiples

let checkDivisible (testNum: int) (diviser: int): bool =
  match diviser with
  | 0 -> false
  | _ -> testNum%diviser = 0
// Seq.exists (function) (sequence/list) :bool

let checkSeqDivisible (divisers: int list) (testNum: int): bool = 
  Seq.exists (checkDivisible testNum) divisers

let keepMultiples (diviser: int) (numbers: int list): (int list) =
  List.filter (fun x -> x%diviser = 0) numbers

let keepMultiples1 (divisers: int list) (numbers: int list): (int list) =
  List.filter (fun x -> List.sum(List.map (fun y -> if x%y = 0 then 1 else 0) divisers) > 0) numbers


let sum1 (numbers: int list) (upperBound: int): int = 
  let allNums = [1..(upperBound-1)]
  
  let keptMultiples = allNums |> keepMultiples(3)

  let loopList (diviserNumbers:int list): int list =
    let rec recLoopList keptMultiples divNumber =
      match divNumber with
      | hd::tl -> 
        let newMultiples = keepMultiples hd [1..(upperBound-1)]
        let unionMultiples = set newMultiples + set keptMultiples
        recLoopList (Seq.toList unionMultiples) tl
      | [] -> keptMultiples
    recLoopList [] diviserNumbers

  let multiplesList = loopList numbers
  let multiplesList1 = keepMultiples1 numbers allNums

  List.fold (fun x -> (+) x ) 0 multiplesList

let sum (numbers: int list) (upperbound: int): int = 
  let allNums = [1..(upperbound-1)]
  let multiplesList = allNums |> List.filter (checkSeqDivisible numbers)
  List.fold (fun x -> (+) x ) 0 multiplesList
//  allnums
//  |> list.filter
//    (fun num -> 
//      numbers |> seq.exists (fun x -> num%x))
  

let sumOfMultiples numbers max = 
    [1..max-1]
    |> Seq.filter 
        (fun candidate -> 
            numbers |> Seq.exists (fun nb -> candidate % nb = 0))
    |> Seq.sum