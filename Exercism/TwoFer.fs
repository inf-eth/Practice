module TwoFer

let twoFer (input: string option): string = 
  match input with
  | Some name -> "One for " + name + ", one for me."
  | None -> "One for you, one for me."
