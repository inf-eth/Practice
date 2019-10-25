type 'a IList = INode of  Id: int * Hd: 'a * Tl : 'a IList | INil

/// internal data constructor which takes garbage collector as a parameter
/// iCons will be derived from this after gc is defined
let iCons' gcAlloc h t = INode( Id = gcAlloc t, Hd=h, Tl=t)

let iHd x =
    match x with
    | INode(i,h,t) -> Some h
    | _ -> None

let iTl x =
    match x with
    | INode(i,h,t) -> Some t
    | _ -> None


let iId = function
    | INode(i, h, t) -> i
    | INil -> -1

let (|IMatch|_|) = function
    | INode (i, h, t) -> Some (h,t)
    | _ -> None


type Data = float IList // Type of all data used in simulations

/// type of stack frame containing data referenced by one function call
/// each stack frame contains a fixed number of name, Data associations for the values of local variables
/// the variables values are mutable
/// garbage collection will assume that all data accessible from a stack frame is alive
type StackFrame = Map<string, Data Ref> 

/// global stack of stack frames used by garbage collections
/// each function call will push a new frame by consing to the head of the list
/// each function return will pop a stack frame by taking the tail of the list
let mutable GlobalStack: StackFrame list = [] // empty stack to begin with

/// create a two parameter stack oriented function by wrapping the provided functionUsingStack
/// the stack frame creation and deletion is handled here. The wrapped function: functionUsingStack 
/// is given the current stack stack frame and the created function as parameters.
/// Note that it must be given the created function in case it needs to implement recursive function calls
/// in simple use cases the function parameters are the only local data needed and localNames = [].
/// the return type is Data -> Data -> Data (a two-parameter function)
let ManageCallTwoParameters p1Name p2Name localNames (functionUsingStack: StackFrame -> (Data -> Data -> Data) -> Data) =
    let rec innerFunc p1 p2 = // this is the function returned
        let oldStack = GlobalStack
        /// stack frame with locals initialised from parameters given by calling function
        /// extra local variables (if needed) are initialised to INil
        let newFrame = Map.ofList <| [ p1Name, ref p1 ; p2Name, ref p2 ] @ (List.map (fun lName -> lName, ref INil) localNames) // create the new stack frame for this function call
        GlobalStack <- newFrame :: oldStack
        let returnedData = functionUsingStack newFrame innerFunc
        GlobalStack <- oldStack // destroy the stack frame for this call which is no longer needed
        returnedData
    innerFunc

//-----------------------------------
// demo 'simplest' storage management
//-----------------------------------

/// simple simulated memory allocation using infinite memory
let nextGenerator() =
    let mutable currentId = 0
    let next() =
        currentId <- currentId + 1
        currentId
    next
/// nextId is the generator function used by iCons
let nextId = nextGenerator()

let checkConsistency (lst:Data) =
    let rec getIdListFromData: Data -> int list = function
        | INode(id,h,t) -> id :: getIdListFromData t
        | _ -> []
    let aliveIdList =
       GlobalStack
       |> List.collect (Map.toList >> List.map (fun (_, x) -> !x) >> List.collect getIdListFromData)
    let checkAlive id =
        match List.exists ((=) id) aliveIdList with
        | true -> () // normal case
        | false -> failwithf "Inconsistent data id %d discovered, id not on stack %A" id aliveIdList
    List.iter checkAlive (getIdListFromData lst)
    
// this function returns the id of a new IList cell guaranteed to be free
// in the simulation each id represents a possible list cell sized element of heap
// lst will be the tail of the cell, and therefore all its elements must be alive
// this function can check for consistency that all elements of lst are reachable from GlobalStack
// it can perform garbage collection to find free ids, using a fixed size heap of ids
// or it can just allocate new ids without even doing garbage collection
let gcAlloc lst =
    checkConsistency lst
    let n = nextId()
    printfn "Creating Data id=%d" n
    n

let iCons = iCons' gcAlloc

printfn "next:%A\n" nextId
printfn "next:%A\n" nextId

let testList:float IList = INode(Id=1,Hd=2.1,Tl=INode(Id=2,Hd=2.2,Tl=INil))

printfn "testList:%A" testList

checkConsistency testList