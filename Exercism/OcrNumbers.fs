module OcrNumbers
let ocr0 = 
    [ " _ ";
      "| |";
      "|_|";
      "   " ]
let ocr1 = 
    [ "   ";
      "  |";
      "  |";
      "   " ]
let ocr2 = 
    [ " _ ";
      " _|";
      "|_ ";
      "   " ]
let ocr3 = 
    [ " _ ";
      " _|";
      " _|";
      "   " ]
let ocr4 = 
    [ "   ";
      "|_|";
      "  |";
      "   " ]
let ocr5 = 
    [ " _ ";
      "|_ ";
      " _|";
      "   " ]
let ocr6 = 
    [ " _ ";
      "|_ ";
      "|_|";
      "   " ]
let ocr7 = 
    [ " _ ";
      "  |";
      "  |";
      "   " ]
let ocr8 = 
    [ " _ ";
      "|_|";
      "|_|";
      "   " ]
let ocr9 = 
    [ " _ ";
      "|_|";
      " _|";
      "   " ]

let ocr2Num (ocr: string List) =
  printfn "%A" ocr
  match ocr with
  | ocr' when ocr' = ocr0 -> '0'
  | ocr' when ocr' = ocr1 -> '1'
  | ocr' when ocr' = ocr2 -> '2'
  | ocr' when ocr' = ocr3 -> '3'
  | ocr' when ocr' = ocr4 -> '4'
  | ocr' when ocr' = ocr5 -> '5'
  | ocr' when ocr' = ocr6 -> '6'
  | ocr' when ocr' = ocr7 -> '7'
  | ocr' when ocr' = ocr8 -> '8'
  | ocr' when ocr' = ocr9 -> '9'
  | _ -> '?'

let validateInput input = ((input |> List.length) % 4 = 0) && ((input |> List.head |> String.length) % 3 = 0)

let getRows input = (input |> List.length) / 4

let getCols input = (input |> List.head |> String.length) / 3

let filterIndices (indiceslst: int list) lst =
  indiceslst |> List.map (fun x -> lst |> List.item x)

let getRow input nRow =
  let indices = [(4*nRow)..(4*nRow+3)]
  let output = input |> filterIndices indices
  output

let getSingleOCR input nRow nCol =
  let row = getRow input nRow
  let indices = [(3*nCol)..(3*nCol+2)]
  row |> List.map (fun x -> x |> Seq.toList |> filterIndices indices |> Seq.toArray |> System.String)

let convertOCRs input = 
  let nRows = getRows input
  let nCols = getCols input
  let rowIndices = [0..(nRows-1)] |> List.replicate nCols |> Seq.collect (fun x -> x) |> Seq.sort |> Seq.toList
  let colIndices = [0..(nCols-1)] |> List.replicate nRows |> Seq.collect (fun x -> x) |> Seq.toList
  List.map2 (fun x y -> (getSingleOCR input x y)) rowIndices colIndices

let convertOCRs2Num inputOCRs =
  inputOCRs |> List.map ocr2Num |> List.toArray |> System.String

let insertCommas input numString =
  let splitStringList = numString |> Seq.splitInto (getRows input) |> Seq.toList |> List.map (fun x -> x |> System.String)
  splitStringList
  |> List.indexed
  |> List.map 
    (fun x -> 
      match x with
      | x,y when x=(getRows input)-1 -> y
      | _,y -> y+",")
  |> List.collect (fun x -> x |> Seq.toList) |> Seq.toArray |> System.String
  
let convert input =
  match validateInput input with
  | true -> Some (input |> convertOCRs |> convertOCRs2Num |> insertCommas input)
  | false -> None