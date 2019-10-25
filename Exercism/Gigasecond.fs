module Gigasecond
open System

let gigasecond = new TimeSpan(0, 0, 0, (int)(10.**9.));

let add (beginDate: DateTime): DateTime = beginDate+gigasecond