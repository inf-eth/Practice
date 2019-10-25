open System.IO

let testString = "yyrrrrrjj"
let infileName = "H:\Visual Studio 2015\Projects\FSharpPractice\WordProcessor\strings.txt"
let outfileName = testString + ".txt"

let FileContents = Seq.toList (File.ReadAllLines(infileName))

printfn "FileContents: %A" FileContents

let astring = "ydfaflafdserfiowefa"

let sort_string str = 
  let listStr = Seq.toList str
  List.sort listStr

let remove_duplicates_str str =
  let listStr = Seq.toList str
  List.distinct listStr

let reduce_string str = 
  str
  |> sort_string |> remove_duplicates_str

let compare_reduced_strings str1 str2 = 
  if reduce_string str1 = reduce_string str2
  then true
  else false

let SearchContents = List.filter (compare_reduced_strings testString) FileContents
let WriteFileContents = Seq.toArray SearchContents
File.WriteAllLines(outfileName,SearchContents)

///printfn "Sorted string: %A" (sort_string astring)
printfn "SearchContents: %A" SearchContents
