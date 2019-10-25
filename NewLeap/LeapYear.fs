module LeapYear
//on every year that is evenly divisible by 4
//  except every year that is evenly divisible by 100
//    unless the year is also evenly divisible by 400

type year = Year of int
type divisor = Divisor of int

let divisibleBy (Divisor d) (Year y)=
  y%d = 0

let isLeapYear y =
  if (Year y)|> divisibleBy (Divisor 400) then true else
  if (Year y) |> divisibleBy (Divisor 100) then false else
  if (Year y) |> divisibleBy (Divisor 4) then true else false

