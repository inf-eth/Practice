module CollatzConjecture

let (|IsEven|_|) x =
  match x with
  | num when x%2 = 0 -> Some x
  | _ -> None

let (|IsOdd|_|) x =
  match x with
  | num when x%2 <> 0 -> Some x
  | _ -> None

let (|IsPositive|_|) x =
  match x with
  | num when x > 0 -> Some num
  | _ -> None

let collatz i =
  let rec collatzr nacc acc =
    match acc with
    | 1 -> nacc
    | IsEven n -> collatzr (nacc+1) (n/2)
    | IsOdd n -> collatzr (nacc+1) (3*n+1)
    | _ -> failwithf "Something wrong"
  collatzr 0 i

let steps (number: int): int option =
  match number with
  | IsPositive i -> Some (collatz i)
  | _ -> None