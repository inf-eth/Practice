let divide ifZero ifSuccess top bottom = 
    if (bottom=0) 
    then ifZero()
    else ifSuccess (top/bottom)
    
let isEven ifOdd ifEven aNumber = 
    if (aNumber % 2 = 0)
    then aNumber |> ifEven 
    else aNumber |> ifOdd 

// setup the functions to print a message
let ifZero1 () = printfn "bad"
let ifSuccess1 x = printfn "good %i" x

// use partial application
let divide1  = divide ifZero1 ifSuccess1

//test
let good1 = divide1 6 3
let bad1 = divide1 6 

42 |>
    (
       fun x -> 43 |> 
       (
         fun y -> x + y |> 
         (
           fun z -> z
         )
       )
    )

