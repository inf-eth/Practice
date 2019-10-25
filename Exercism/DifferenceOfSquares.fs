module DifferenceOfSquares

let square (number: int): int = number * number

let squareOfSum (number: int): int = [1..number] |> List.sum |> square

let sumOfSquares (number: int): int = [1..number] |> List.map (square) |> List.sum

let differenceOfSquares (number: int): int = abs ((sumOfSquares number) - (squareOfSum number))