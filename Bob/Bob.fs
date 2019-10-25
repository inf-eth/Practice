module Bob

type talktype = 
  | Question
  | Yell
  | Nothing
  | Whatever

let isSpace c =
  match c with
  | ' ' -> true
  | _ -> false

let isNOTSpace c = not (isSpace c)

let isLetter c =
  let smallLetters = ['a'..'z']
  let capitalLetters = ['A'..'Z']
  let letters = smallLetters @ capitalLetters
  match List.tryFind (fun x->c=x) letters with
  | Some x -> true
  | None -> false

let isNOTLetter c = not (isLetter c)

let isLower c =
  let smallLetters = ['a'..'z']
  match List.tryFind (fun x->c=x) smallLetters with
  | Some x -> true
  | None -> false

let isUpper c =
  let capitalLetters = ['A'..'Z']
  match List.tryFind (fun x->c=x) capitalLetters with
  | Some x -> true
  | None -> false

let isNOTUpper c = not (isUpper c)

let isQuestionMark c =
  c = '?'

let processTalk talk =
  let lst_spaces = Seq.toList talk
  let lst = lst_spaces |> List.filter (isNOTSpace)
  printfn "noSpaces: %A" lst
  match List.rev lst with
  | [] -> Nothing
  | tl::hd ->
               let lettersOnly = lst |> List.filter (isLetter)
               printfn "lettersOnly: %A" lettersOnly
               let capsRemoved = lettersOnly |> List.filter (isNOTUpper)
               printfn "capsRemoved: %A" capsRemoved
               let flag =
                          match lettersOnly with
                          | [] -> false
                          | _ -> true
               match capsRemoved with
               | [] when flag = true -> Yell
               | _ -> match tl with
                      | '?' -> Question
                      | _ -> Whatever

let hey talk =
  match processTalk talk with
  | Question -> "Sure."
  | Yell -> "Whoa, chill out!"
  | Nothing -> "Fine. Be that way!"
  | Whatever -> "Whatever."

printfn "%A" (hey "ABC 123?") |> ignore
