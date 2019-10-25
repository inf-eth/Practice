module Clock

let getActualMinutes clock =
  match clock%1440 with
  | minutes when minutes >= 0 -> minutes
  | minutes when minutes < 0 -> minutes+1440
  | _ -> failwithf "unknown minutes"

let create hours minutes = getActualMinutes (hours*60 + minutes)

let add minutes clock = getActualMinutes (clock + minutes)

let subtract minutes clock = getActualMinutes (clock - minutes)

let display clock =
  let hours = clock/60
  let minutes = clock%60
  sprintf "%02d:%02d" hours minutes
