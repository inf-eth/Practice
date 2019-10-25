module NucleotideCount

let addNucleotide (strandMap: Map<char, int>) (nucleotide: char) =
  strandMap.Add(nucleotide,((strandMap.[nucleotide])+1))

let mapStrand (strand: string): Map<char, int> =
  let strandList = strand |> Seq.toList
  let nucleotideList = [('A',0);('C',0);('G',0);('T',0)]
  let initialMap = nucleotideList |> Map.ofList
  let rec mapStrandTR accMap strandListacc =
    match strandListacc with
    | [] -> accMap
    | hd::tl -> (mapStrandTR (addNucleotide accMap hd) tl)
  mapStrandTR initialMap strandList

let nucleotideCounts (strand: string): Option<Map<char, int>> =
  match strand |> Seq.toList |> List.filter (fun x -> x<>'A' && x<>'G' && x<>'T' && x<>'C') with
  | [] -> Some (mapStrand strand)
  | _ -> None
