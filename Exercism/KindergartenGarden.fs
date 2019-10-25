module KindergartenGarden

// TODO: define the Plant type
type Plant = | Grass | Clover | Radishes | Violets
let translatePlant plantChar =
  match plantChar with
  | 'G' -> Plant.Grass
  | 'C' -> Plant.Clover
  | 'R' -> Plant.Radishes
  | 'V' -> Plant.Violets
  | _ -> failwithf "unkown plant type"

let plants (diagram: string) student =
  let listOfStudents = ["Alice";"Bob";"Charlie";"David";"Eve";"Fred";"Ginny";"Harriet";"Ileana";"Joseph";"Kincaid";"Larry"] |> List.sort
  let findStudentIndex (st: string) (studentList: string list): int = studentList |> List.findIndex (fun s -> s=st)
  let studentIndex = findStudentIndex student listOfStudents
  let gardenLength = diagram.IndexOf "\n"
  let studentPlants = [diagram.[studentIndex*2]; diagram.[studentIndex*2+1]; diagram.[studentIndex*2+gardenLength+1]; diagram.[studentIndex*2+gardenLength+2]]
  let plantsList = studentPlants |> List.map translatePlant
  printfn "%A" gardenLength
  printfn "%A" plantsList
  plantsList
