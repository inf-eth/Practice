module Gigasecond
open System

let gigasecond input =
  input

let date1 = DateTime(2000, 1, 1)
let date2 = DateTime(2001, 1, 1)
let dateDiff =  date2 - date1
printfn "Total days between %A and %A: %A" date1 date2 dateDiff.TotalDays
