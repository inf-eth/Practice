module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let create direction position = {direction=direction; position=position}

let turnRight initialDirection =
  match initialDirection with
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let turnLeft initialDirection =
  match initialDirection with
  | North -> West
  | East -> North
  | South -> East
  | West -> South

let advance direction position =
  let x,y = position
  match direction with
  | North -> x,(y+1)
  | East -> (x+1),y
  | South -> x,(y-1)
  | West -> (x-1),y

let getDirection initialDirection providedDirection =
  true

let step instruction (robot: Robot) = 
  match instruction with
  | 'R' -> {direction=turnRight robot.direction; position=robot.position}
  | 'L' -> {direction=turnLeft robot.direction; position=robot.position}
  | 'A' -> {direction=robot.direction; position=advance robot.direction robot.position}
  | _ -> failwithf "unknown instruction"

let move instructions robot = 
  let instructionsList = instructions |> Seq.toList
  let rec executeInstructionsList accInstructions accRobot =
    match accInstructions with
    | [] -> accRobot
    | hd::tl -> executeInstructionsList tl (step hd accRobot)
  executeInstructionsList instructionsList robot
