//module LeapYear

let private divisibleBy b a = 
  match a%b with
  | 0 -> true
  | _ -> false

let private (|DivisibleBy|_|) d nb = 
    if nb |> divisibleBy d
    then Some DivisibleBy 
    else None

let isLeapYear year = 
    match year with
    | DivisibleBy 400 -> true
    | DivisibleBy 100 -> false
    | DivisibleBy 4   -> true
    | _               -> false

let x = isLeapYear 1996

type temp = 
  | DegreeC of float
  | DegreesF of float
  | None

let z:temp = DegreeC 41.2
