// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//namespace mainprog
open System
open SumOfMultiples
open KindergartenGarden
open Clock
open TreeBuilding
open NucleotideCount

let test = SumOfMultiples.sum [1] 100
printfn "%d" test

//let x = SumOfMultiples.checkDivisible 3 0
//printfn "%A" x

printfn "%A" (SumOfMultiples.checkSeqDivisible [0;4] 3)

let student = "Alice"
let diagram = "RC\nGG"
let expected = [Plant.Radishes; Plant.Clover; Plant.Grass; Plant.Grass]
printfn "%A" (plants diagram student)
printfn "%A" (Clock.display 3605)

let input = [{ RecordId = 3; ParentId = 0 };{ RecordId = 2; ParentId = 0 };{ RecordId = 1; ParentId = 0 };{ RecordId = 0; ParentId = 0 }]
let tree = buildTree input
let tree1 = buildTree1 input


// Define a simple type which has fields that can be validated
type Request = { Name: string; Email: string }

// Define some logic for what defines a valid name.
//
// Generates a Result which is an Ok if the name validates;
// otherwise, it generates a Result which is an Error.
let validateName req =
    match req.Name with
    | null -> Error "No name found."
    | "" -> Error "Name is empty."
    | "bananas" -> Error "Bananas is not a name."
    | _ -> Ok req

// Similarly, define some email validation logic.
let validateEmail req =
    match req.Email with
    | null -> Error "No email found."
    | "" -> Error "Email is empty."
    | s when s.EndsWith("bananas.com") -> Error "No email from bananas.com is allowed."
    | _ -> Ok req

let validateRequest reqResult =
    reqResult 
    |> Result.bind validateName
    |> Result.bind validateEmail

let test1() = 
    // Now, create a Request and pattern match on the result.
    let req1 = { Name = "Phillip"; Email = "phillip@contoso.biz" }
    let res1 = validateRequest (Ok req1)
    match res1 with
    | Ok req -> printfn "My request was valid! Name: %s Email %s" req.Name req.Email
    | Error e -> printfn "Error: %s" e
    // Prints: "My request was valid!  Name: Phillip Email: phillip@consoto.biz"

    let req2 = { Name = ""; Email = "phillip@bananas.com" }
    let res2 = validateRequest (Ok req2)
    match res2 with
    | Ok req -> printfn "My request was valid! Name: %s Email %s" req.Name req.Email
    | Error e -> printfn "Error: %s" e
    // Prints: "Error: No email from bananas.com is allowed."

test1 ()

