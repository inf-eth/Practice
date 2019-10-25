open Raindrops
open GradeSchool
open ArmstrongNumbers
open Allergies
open OcrNumbers

module Program =
  let [<EntryPoint>] main _ =
    printfn "%A" (Raindrops.convert 1)
    let school = studentsToSchool [("Aimee", 2)]
    printfn "%A" school
    printfn "%A" (countDigits 1)
    isArmstrongNumber 6
    printfn "%A" (Allergies.list 3)

    let rows = 
        [ "       _     _        _  _ ";
          "  |  || |  || |  |  || || |";
          "  |  ||_|  ||_|  |  ||_||_|";
          "                           " ]
    //printfn "%A" (convertOCRs rows)
    //printfn "%A" (getSingleOCR rows 0 8)
    printfn "%A" (convert rows)
    let stringtest = "abcdefgh"
    let splitstring = (stringtest |> Seq.splitInto 9 |> Seq.toList)// |> List.map (fun x-> x |> Seq.toArray |> System.String) |> Seq.collect (fun x -> x) |> Seq.toArray |> System.String)
    printfn "%A" splitstring
    0
