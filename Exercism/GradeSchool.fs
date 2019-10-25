module GradeSchool
type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
  let oldGrade = school.TryFind grade
  match oldGrade with
  | Some foundGrade ->
                  let newGrade = [student] @ foundGrade |> List.sort
                  school.Remove(grade).Add (grade,newGrade)
  | None ->
            let newGrade = [student]
            school.Add (grade,newGrade)
  
let roster (school: School): string list =
  school |> Map.toList |> List.map (fun (i,s) -> s) |> List.collect (fun sList -> sList)

let grade (number: int) (school: School): string list =
  let foundGrade = school.TryFind number
  match foundGrade with
  | Some found -> school.[number]
  | None -> []

let studentsToSchool (students: List<string*int>):School =
    let schoolFolder school (name,grade) =
        add name grade school
    List.fold schoolFolder empty students
