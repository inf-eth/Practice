module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree = 
    | Branch of int * Tree list
    | Leaf of int

let recordId t = 
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t = 
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t = 
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let buildLeafs records =
  let rec recBuildLeafs accLeafs accRecords =
    match accRecords with
    | [] -> accLeafs
    | hd::tl when hd.RecordId = 0 -> recBuildLeafs ((-1,hd.RecordId)::accLeafs) tl
    | hd::tl -> recBuildLeafs ((hd.ParentId,hd.RecordId)::accLeafs) tl
    | _ -> failwith "shouldn't be here"
  recBuildLeafs [] records

let buildTree records = 
  match List.sortBy (fun x -> x.RecordId) records with
  | [] -> failwith "Empty input"
  | records' when records'.[0].ParentId <> 0 || records'.[0].RecordId <> 0 -> failwith "Root node is invalid"
  | records' when records' |> List.exists (fun r -> (r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId))) -> failwith "Nodes with invalid parents"
  | records' when records' |> List.map (fun r -> r.RecordId) <> [0..((List.length records')-1)] -> failwith "Non-continuous list"
  | records' -> 
                let leafs = (buildLeafs records') |> List.rev
                printfn "%A" (leafs |> List.rev)
                let root = leafs.[0]

                let grouped = leafs |> List.groupBy fst |> List.map (fun (x, y) -> (x, List.map snd y))
                let parens = List.map fst grouped
                let map = grouped |> Map.ofSeq

                let rec helper key =
                    if Map.containsKey key map then
                        Branch (key, List.map (fun i -> helper i) (Map.find key map))
                    else
                        Leaf key                    

                let root = helper 0
                root

let buildTree1 records = 
    let records' = List.sortBy (fun x -> x.RecordId) records

    if List.isEmpty records' then failwith "Empty input"
    else
        let root = records'.[0]
        if (root.ParentId = 0 |> not) then
            failwith "Root node is invalid"
        else
            if (root.RecordId = 0 |> not) then failwith "Root node is invalid"
            else
                let mutable prev = -1
                let mutable leafs = []

                for r in records' do
                    if (r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) then
                        failwith "Nodes with invalid parents"
                    else
                        if r.RecordId <> prev + 1 then
                            failwith "Non-continuous list"
                        else                            
                            prev <- r.RecordId
                            if (r.RecordId = 0) then
                                leafs <- (-1, r.RecordId) :: leafs
                            else
                                leafs <- (r.ParentId, r.RecordId) :: leafs
                printfn "%A" leafs
                leafs <- List.rev leafs 
                let root = leafs.[0]

                let grouped = leafs |> List.groupBy fst |> List.map (fun (x, y) -> (x, List.map snd y))
                let parens = List.map fst grouped
                let map = grouped |> Map.ofSeq

                let rec helper key =
                    if Map.containsKey key map then
                        Branch (key, List.map (fun i -> helper i) (Map.find key map))
                    else
                        Leaf key                    

                let root = helper 0
                root