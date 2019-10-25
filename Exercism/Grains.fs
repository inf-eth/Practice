module Grains
open System.Numerics

let doubleSequence (initial: uint64) (n: int): uint64 list =
  let rec doubleSequenceTR accSeq accn =
    match accSeq with
    | [] -> failwith "empty initial state"
    | _ when accn = 1 -> accSeq
    | _ when accn < 1 -> failwith "count less than 1"
    | hd::tl -> doubleSequenceTR ((hd*2UL)::accSeq) (accn-1)
  doubleSequenceTR [initial] n

let square (n: int): Result<uint64,string> =
  match n with
  | n' when n' > 0 && n' < 65 -> Ok (uint64 (BigInteger.Pow (BigInteger 2, n-1)))
  | _ -> Error "square must be between 1 and 64"

let total: Result<uint64,string> =
  let sum = (doubleSequence 1UL 64) |> Seq.sum
  Ok sum
