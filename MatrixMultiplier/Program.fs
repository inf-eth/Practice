open System

let nRowsA = 2
let nColsA = 2
let nRowsB = 2
let nColsB = 1

let getCol mat j = 
  let length = Array2D.length1 mat
  let colj = Array.create length 0
  for k in [0..length-1] do
   colj.[k] <- mat.[k,j]
  colj

let getRow mat i = 
  let length = Array2D.length2 mat
  let rowi = Array.create length 0
  for k in [0..length-1] do
   rowi.[k] <- mat.[i,k]
  rowi

let getCol1 (mat:'a[,]) j = 
  mat.[*,j]

let getRow1 (mat:'a[,]) i = 
  mat.[i,*]

let scalarProduct A B =
  let mutable sum = 0
  if Array.length A = Array.length B then
    for i in [0..(Array.length A)-1] do
      sum <- sum + A.[i] * B.[i]
  else failwithf "array lengths don't match"
  sum

let scalarProduct1 A B =
  let multFolder state elementA elementB = elementA*elementB+state
  Array.fold2 multFolder 0 A B



let matrixMultiplication A B = Array2D.init (Array2D.length1 A) (Array2D.length2 B) (fun i j -> scalarProduct (getRow A i) (getCol B j))
let matrixMultiplication1 A B = Array2D.init (Array2D.length1 A) (Array2D.length2 B) (fun i j -> scalarProduct1 (getRow1 A i) (getCol1 B j))
let matrixMultiplication2 A B = Array2D.init (Array2D.length1 A) (Array2D.length2 B) (fun i j -> scalarProduct1 A.[i,*] B.[*,j])

let matA = Array2D.init nRowsA nColsA (fun i j -> if i=j then i*j else i+j)
let matB = Array2D.init nRowsB nColsB (fun i j -> if i=j then 2*i*j else i+2*j)

printfn "get row 2 of mat A: %A" (getRow matA 1)

printfn "Mat A is: %A" matA
printfn "Mat b is: %A" matB

let scalarProduct1D (A:int[]) (B:int[]) =
  Array.init (Array.length A) (fun i -> A.[i]*B.[i])

let scalarProduct2D A B =
  Array2D.init (Array2D.length1 A) (Array2D.length2 B) (fun i j -> A.[i,j]*B.[i,j])

type MatType = 
  | OneDMat of int[]
  | TwoDMat of int[,]
  | ThreeDMat of int[,,]
  | FourDMat of int[,,,]

let inline (.*) A B :MatType = 
  match (A,B) with
  | (OneDMat A,OneDMat B) -> OneDMat (scalarProduct1D A B)
  | (TwoDMat A,TwoDMat B) -> TwoDMat (scalarProduct2D A B)
  | _ -> failwithf "unknown matrix type"

let A_ = OneDMat [|1; 3; 4; 0|]
let B_ = OneDMat [|4; 1; 2; 9|]
printfn "%A" A_
printfn "%A * %A = %A" A_ B_ (A_.*B_)

//List.reduce (+) (for k in [1..nColsA] -> matA.[k,k])

let matC = Array2D.init nRowsA nColsB (fun i j ->
  let mutable sum = 0
  for k in [0..nColsA-1] do
    sum <- sum + matA.[i,k]*matB.[k,j]
  sum)

let matD = Array2D.init nRowsA nColsB (fun i j -> scalarProduct (getRow matA i) (getCol matB j))

let matE = matrixMultiplication matA matB
let matF = matrixMultiplication2 matA matB

printfn "Mat C is: %A" matC
printfn "Mat D is: %A" matD
printfn "Mat E is: %A" matE
printfn "Mat F is: %A" matF

type 'a IList = INode of Id: int * Hd: 'a * Tl: 'a IList | INil

let iHd = function
  | INode(i,h,t) -> Some h
  | _ -> None

let iTl = function
  | INode(i,h,t) -> Some t
  | _ -> None

let iId = function
  | INode(i,h,t) -> i
  | INil -> -1

let iCons h t = INode(-1, h, t)

let mutable testList = iCons 0 INil
testList <- iCons 1 testList
testList <- iCons 2 testList
testList <- iCons 3 testList
testList <- iCons 4 testList
testList <- iCons 5 testList

printfn "testList: %A" testList

let (|IMatch|_|) x = 
  match x with
  | INode(i,h,t) -> Some (h,t)
  | _ -> None

let getHT = function
  | IMatch(h,t) -> (h,t)
  | _ -> failwithf "shouldn't be here"

printfn "head and tail of testList: %A" (getHT testList)

(*
// Cleaned up code for Matrix multiplication
let scalarProduct A B =
  let multFolder state elementA elementB = elementA*elementB+state
  Array.fold2 multFolder 0 A B

let matrixMultiplication A B = Array2D.init (Array2D.length1 A) (Array2D.length2 B) (fun i j -> scalarProduct A.[i,*] B.[*,j])

let matA = Array2D.init nRowsA nColsA (fun i j -> if i=j then i*j else i+j)
let matB = Array2D.init nRowsB nColsB (fun i j -> if i=j then 2*i*j else i+2*j)

let matF = matrixMultiplication matA matB
printfn "Mat F is: %A" matF
*)
