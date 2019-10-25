// WS 3

type A1 = | Monday1 of Unit | Tuesday1 of Unit
type A2 = | Monday2 | Tuesday2

let snd3 x =
  match x with
  | _,_,a -> a

(1,2,3) |> snd3 |> printfn "3rd selected is: %A"

// Q9
type 'a DUBTree = | BTreeNode of ('a * 'a DUBTree * 'a DUBTree) | Empty

let ABTree:int DUBTree = BTreeNode(1,BTreeNode(2,Empty,Empty),BTreeNode(3,Empty,Empty))

ABTree |> printfn "printing ABtree: %A"

type RList = | RNode of Head: int * Tail: RList | Empty

let ARList:RList = RNode (Head=1, Tail=RNode(2,RNode(3,Empty)))

ARList |> printfn "printing RList: %A"

let inline (+?) (x: int) (y: RList) = RNode (Head=x, Tail=y)

let consRList = 99+?ARList

consRList |> printfn "printing consRList: %A"

let betterSqrt x = if x < 0.0 then None else Some (sqrt x)
betterSqrt -0.1 |> printfn "sqrt of -0.1: %A"

type EEERecord = {eid:int; name:string; role:string; year:int; cid:int}

open FsCheck
    
let revOfRevIsOrig (x: float list) = List.rev (List.rev x) = x
    
Check.Quick revOfRevIsOrig

let plusIsAssociative a b c = 
       (a + b) + c = a + (b + c)
    
Check.Quick plusIsAssociative

let ``de Morgan's Theorem Fails`` a b = 
    (a && b) = not ((not a) || (not b))
    
Check.Quick ``de Morgan's Theorem Fails``
    
let associativity (x:int) (f:int->float,g:float->char,h:char->int) =
    ((f >> g) >> h) x = (f >> (g >> h)) x
        
Check.Quick associativity
    
type ListProperties = // define set of properties as members of a user-defined type
  static member ``reverse of reverse is original`` (xs: int list) = 
         List.rev(List.rev xs) = xs
  static member ``reverse is original`` (xs: int list) = 
         List.rev xs = xs
         
Check.QuickAll<ListProperties>()