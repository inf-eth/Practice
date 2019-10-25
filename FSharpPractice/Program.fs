// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
// Deck of cards
(*
type suit = | Club | Diamond | Heart | Spade
type rank = | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type card = {Rank:rank;Suit:suit}
let card1 = {Rank=One;Suit=Diamond}
let card2 = {Rank=Queen;Suit=Diamond}
let card3 = {Rank=Ten;Suit=Spade}
let sortCards cards =
  cards |> List.sortDescending

printfn "card1, card2 and card3 sorted: %A" (sortCards ([card1;card2;card3]))

let compare_xy (xy: int*int) = 
  match xy with
  | (0, _) -> "x is zero"
  | (_, 0) -> "y is zero"
  | _ -> "x and y are different than zero"
printfn "%A" (compare_xy (0,0))

let x = 3
printfn "value of x is %A" x

let indexArray = [|2; 3; 4|] |> Array.indexed
let iterArray = [|2; 3; 4|] |> Array.iter (fun x -> printfn "%d" x)

printfn "Array.indexed = %A" indexArray
printfn "Array.iter = are %A" iterArray

//[<EntryPoint>]
//let main argv = 
//    printfn "%A" argv
//    0 // return an integer exit code

open System // for Math.Ceiling
let primesSieve upTo =
   // array of booleans. Index 0 and 1 are not used.
   let primeSieve = Array.create (upTo+1) true
   printfn "primeSieve = %A" primeSieve
   // Largest prime factor we need to sieve by
   let maxPrimeFactor = upTo |> float |> sqrt |> Math.Ceiling |> int
   printfn "maxPrimeFactor = %A" maxPrimeFactor
   // Return array of primes from the current value of primeSieve
   let sieveToPrimes ar = 
      ar |> printfn "current ar:%A"
      ar |> Array.indexed |> Array.collect (fun (i,b)-> if i >= 2 && b then [|i|] else [||]) 
   // set elements with indexes multiples of p to false in primeSieve array.
   let removeMultiples p =
      if primeSieve.[p]
      then [| p*p..p..upTo|] 
           |> Array.iter (fun p -> primeSieve.[p] <- false)

   [|2..maxPrimeFactor|] |> Array.iter (fun p -> removeMultiples p)// do all the prime factor sieving   
   sieveToPrimes primeSieve

let primesPiped upTo =
   // array of booleans. Index 0 and 1 are not used.
   //let primeSieve = Array.create (upTo+1) true
   //printfn "primeSieve = %A" primeSieve
   // Largest prime factor we need to sieve by
   let maxPrimeFactor = upTo |> float |> sqrt |> Math.Ceiling |> int
   printfn "maxPrimeFactor = %A" maxPrimeFactor
   // Return array of primes from the current value of primeSieve
   let sieveToPrimes ar = 
      ar |> printfn "current ar:%A"
      ar |> Array.indexed |> Array.collect (fun (i,b)-> if i >= 2 && b then [|i|] else [||]) 
   // set elements with indexes multiples of p to false in primeSieve array.
   let removeMultiples (sieve:bool []) (p:int) =
      if sieve.[p]
      then [| p*p..p..upTo|] 
           |> Array.iter (fun p -> sieve.[p] <- false)

   let interMultiples sieve = 
     [|2..maxPrimeFactor|] |> Array.iter (fun p -> removeMultiples sieve p)
     sieve

   Array.create (upTo+1) true |> interMultiples |> sieveToPrimes

let limit = 10
printfn "primes upto %d are: %A" limit (primesPiped limit)

let checkMultiple p = 
  let modfunc x = x % p > 0
  modfunc

let rec filterNonPrimesTr lst =
    let rec filterNonPrimestr res primes = 
        match res with
        | p :: xs -> filterNonPrimestr (List.filter (checkMultiple p) xs) (primes @ [p])
        | []      -> primes
    filterNonPrimestr lst []

let rec filterNonPrimes lst =
    match lst with
    | (p::xs) -> p :: filterNonPrimes (List.filter (checkMultiple p) xs)
    | []      -> []

let primes n = filterNonPrimesTr [2..n]
printfn "primes %A" (primes 10)  // [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47]

// Random question. Inputs a list and then outputs the even indices first followed by odd indices.
let firstofPair lst = 
  match lst with
  | [a;b] -> [a]
  | _ -> []

let secondofPair lst = 
  match lst with
  | [a;b] -> [b]
  | _ -> []

let inList = [1;2;3;4;5;6;7;8]
let evenList = inList |> List.chunkBySize 2 |> List.collect (firstofPair)
let oddList = inList |> List.chunkBySize 2 |> List.collect (secondofPair)
let evenoddGroupedList = evenList @ oddList
evenoddGroupedList |> printfn ": %A"

// Memoise and maps
let xm = Map ["first","the" ; "second","cat"]

let ym = Map ["cat",7 ; "the",3]

let zm = Map [ [1;2;3],5 ]

printfn "zm.[[1;2;3]] %A" zm.[[1 ; 2 ; 3]] // look up key in zm map (returns 5)

let ym' = Map.add "fox" 5 ym // new map with ("fox",5) added to ym.

printfn "xm.[\"first\"]: %A" xm.["first"]

let firstofMapPair mp =
  match mp with
  | (a,b) -> a

xm |> Map.toList |> List.map firstofMapPair |> printfn "xm map %A"

// Q36
let x1 = 5%3
x1 |> printfn "x1: %A"

let rec primeFactors n =
  let c = 2
  let rec primeFactorsTR c n =
    if (n%c) = 0
    then
      [c] @ primeFactors (n/c)
    else
      let c1 = c+1
      if c1*c1 > n
      then
        [n]
      else
        primeFactorsTR c1 n
  primeFactorsTR c n

let factorise = primeFactors 21
factorise |> printfn "prime factors: %A"

// Q37
let memoise fn =
   let mutable cache = Map []
   fun x ->
       if cache.ContainsKey(x) = false
       then
         cache <- cache.Add(x,fn x)
       cache.[x]

let square x = 
   printfn "Square called with x = %A" x
   x*x

let mSquare = memoise square
printfn "%A" (mSquare 10)
printfn "%A" (mSquare 3)
printfn "%A" (mSquare 10) // result is in cache and not recomputed
printfn "%A" (mSquare 3)
let mSquare1 = memoise square
printfn "%A"  (mSquare1 10) // result is computed again because mSquare1 has a different cache from mSquare

// List.fold test
let flst = [1;2;3;4]
let construct plst x =
  x :: plst

List.fold construct [] flst |> printfn "folded flst: %A"

// Taylor series
let fact n:int = 
  let i = [1..n]
  List.reduce (*) i

let power x n = x ** n

let lst1 = [1;2;3;4]
let add1 x y = x+y
printfn "sum of list: %A" (List.fold add1 0 lst1)
printfn "power %A" (power (float 2) (float 3))

let date1 = DateTime(2000, 1, 1)
let date2 = DateTime(2001, 1, 1)
let dateDiff =  date2 - date1
printfn "Total days between %A and %A: %A" date1 date2 dateDiff.TotalDays
printfn "Date diff is: %A" dateDiff
//let Seconds = DateTime(1e9)

type button1 = 
  | On
  | Off

On
Off

type record8 = {name:string;age:int}
let recvar = ref {name="abc";age=3}
//recvar.name := "efg"
!recvar;;

let rec fact2 n =
  match n with
  | 0 | 1 -> 1
  | _ -> n * fact2(n-1)


let add x =
  fun y -> x+y

let add10 = add 10

add10 5;;

let inc n =
  fun x -> x+n

let inc1 = inc 1
inc1 4;;

let d (f: float->float) x =
  let dx = 1e-6 (*sqrt System.Double.Epsilon*)
  (f(x+dx)-f(x-dx))/(2.0*dx)

let f x = x ** 3.0 - x - 1.0
let f' = d f
d f 2.0;;
f 2.0;;
f' 2.0;;


type t1 = int
type t2 = int -> int
type t3 = t1 -> t2
let testA = fun t1 -> (fun t1 -> t1)
let sum x = x+x
let sum1 x = fun y -> x+y
let sum2 sum = sum1
let sum3 x = fun y -> sum2 x y
let sum4 x = 
  fun y -> x y+7

let sum5 x y = x+y

//let testA x = x+1
let testB x y = x+y+1
let testC x = testB x
let testD f = (f 1)+1
let testE x y z = x+y+z
let testF f = testB (f 1)
let testG x f = (f (testA x))+1
let testH f = (f 1 2)+1

//let add x y :float = x + y

let double_epsilon = System.Double.Epsilon

let addThreeNumbers x y z  = 

    //create a nested helper function
    let add n = 
       fun x -> x + n
       
    // use the helper function
    x |> add y |> add z

let factI n =
  let rec tfact n acc =
    match n with
    | 0 -> acc
    | n -> tfact (n-1) (System.Numerics.BigInteger n)*acc
  tfact n 1I

factI 50
*)

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace prog
