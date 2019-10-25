module QueenAttack

let (|IsValidRange|_|) x =
  match x with
  | x when x > -1 && x < 8 -> Some x
  | _ -> None

let create (position: int * int) =
  match position with
  | IsValidRange x, IsValidRange y -> true
  | _ -> false

let canAttack (queen1: int * int) (queen2: int * int) =
  let x2,y2 = queen2
  match queen1 with
  | x1,y1 when x1 = x2 || y1 = y2 -> true
  | x1,y1 when abs (x1-x2) = abs (y1-y2) -> true
  | _ -> false