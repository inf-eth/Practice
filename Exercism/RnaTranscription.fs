module RnaTranscription

let complementNucleotide input = 
  match input with
  | 'G' -> 'C'
  | 'C' -> 'G'
  | 'T' -> 'A'
  | 'A' -> 'U'
  | _ -> failwith "unknown nucleotide"

let toRna (dna: string): string =
  dna |> Seq.toList |> List.map complementNucleotide |> Seq.toArray |> System.String
