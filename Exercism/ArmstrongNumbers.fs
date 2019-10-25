module ArmstrongNumbers

let countDigits (number: int): int =
  let rec countDigitstr accn acc =
    match acc with
    | 0 -> accn
    | _ -> countDigitstr (accn+1) (acc/10)
  countDigitstr 1 (number/10)

let int2String (x: int) = string x
let string2CharList (s: string): char list = List.ofSeq s
let char2Int (c: char) = (int c)-48

let isArmstrongNumber (number: int): bool =
  let nDigits = countDigits number
  let rec computeArmstrongValue accn accArmstrongValue =
    match accn with
    | -1 -> accArmstrongValue
    | _ ->
      let extractedDigit = number/int(10.**(float)accn)%10
      computeArmstrongValue (accn-1) (accArmstrongValue+int((float)extractedDigit**(float)nDigits))
  let armstrongCompute1 = computeArmstrongValue (nDigits-1) 0

  let intListNumber = number |> int2String |> string2CharList |> List.map char2Int
  let armstrongCompute = intListNumber |> List.map (fun x -> (int) (((float)x)**(float)nDigits)) |> List.fold (fun x -> (+) x) 0
  armstrongCompute1 = number