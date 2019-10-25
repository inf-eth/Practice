// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
//module UserData
type UserInfo = {Name: string option; Age: int option}
type Input = {InputName:string; InputAge:string}
type ErrorMessage = {NameStatus: string option; AgeStatus: string option}
type DataFlow =
  | Success of Input
  | Failure of ErrorMessage

type Test =
  | AnInteger of int
  | NotANumber of obj

(*
let (|Integer|_|) (x:int) =
  match x with
  | Some(int32(x)) -> IsInt
  | _ -> NotAnInt *)

let checkInput = function
  | AnInteger(x) -> printfn "You entered: %A" x
  | NotANumber(_) -> printfn "Invalid input"

let IsNameValid name =
  1

let parseInput inputData = 
  let parseName =
    Some(inputData.InputName)
  let parseAge = 
    match System.Int32.TryParse(inputData.InputAge) with
    | (result,x) when result = true -> Some(int32(inputData.InputAge))
    | _ -> None
  (parseName,parseAge)

let test x =
  match x with
  | Some y -> y
  | None -> None

let MakeDataFlow inputUserData = 
  let parsedInputUserData = parseInput inputUserData
  match parsedInputUserData with
  | (Some name, Some age) -> Success(inputUserData)
  | (None, Some age) -> Failure({NameStatus = Some("Invalid input: Name"); AgeStatus = None})
  | (Some name, None) -> Failure({NameStatus = None; AgeStatus = Some("Invalid input: Age")})
  | _ -> Failure({NameStatus = Some("Invalid input: Name"); AgeStatus = Some("Invalid input: Age")})

let CheckUserName dataFlow =
  match dataFlow with
  | Success(dataFlow) -> 1
  | Failure(dataFlow) -> 2

type CheckAge = DataFlow -> DataFlow
type Conclusion = DataFlow -> DataFlow

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    printfn "Enter user name: "
    let InName = System.Console.ReadLine()

    printfn "Enter user Age: "
    let InAge = System.Console.ReadLine()

    let InputUserData = {InputName = InName; InputAge = InAge}

    InputUserData |> MakeDataFlow |> CheckUserName //|> CheckAge |> Conclusion

    0 // return an integer exit code



