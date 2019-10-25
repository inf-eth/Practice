module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
  let rec accumulateRec (acc: 'b list) (input: 'a list) =
    match input with
    | [] -> acc
    | hd::tl -> accumulateRec (acc @ [func hd]) tl
  accumulateRec [] input