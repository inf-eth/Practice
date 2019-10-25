open System 
open System.Windows 
open System.Windows.Controls 
open Utilities
 
type Calculate = CalculatorInput * CalculatorState -> CalculatorState 
and CalculatorState = {
    display: CalculatorDisplay
    pendingOp: (CalculatorMathOp * Number) option
    }
and CalculatorDisplay = string
and CalculatorInput = 
    | Digit of CalculatorDigit
    | Op of CalculatorMathOp
    | Action of CalculatorAction
and CalculatorDigit = 
    | Zero | One | Two | Three | Four 
    | Five | Six | Seven | Eight | Nine
    | DecimalSeparator
and CalculatorMathOp = 
    | Add | Subtract | Multiply | Divide
and CalculatorAction = 
    | Equals | Clear
and UpdateDisplayFromDigit = 
    CalculatorDigit * CalculatorDisplay -> CalculatorDisplay
and DoMathOperation = 
    CalculatorMathOp * Number * Number -> MathOperationResult 
and Number = float
and MathOperationResult = 
    | Success of Number 
    | Failure of MathOperationError
and MathOperationError = 
    | DivideByZero

type GetDisplayNumber = 
    CalculatorDisplay -> Number option
type SetDisplayNumber = 
    Number -> CalculatorDisplay 

type InitState = 
    unit -> CalculatorState 

type CalculatorServices = {
    updateDisplayFromDigit: UpdateDisplayFromDigit 
    doMathOperation: DoMathOperation 
    getDisplayNumber: GetDisplayNumber 
    setDisplayNumber: SetDisplayNumber 
    initState: InitState 
    }

let createCalculate (services:CalculatorServices) :Calculate = 
    fun (input,state) -> 
        match input with
        | Digit d -> state// as above
        | Op op -> state// to do
        | Action Clear ->
            let newState = services.initState()
            newState //return
        | Action Equals ->
            let newState = updateDisplayFromPendingOp services state
            newState //return

// create the services
let services = CalculatorServices.createServices()

// inject the services into the "factory" method
let calculate = CalculatorImplementation.createCalculate services

// the returned "calculate" function is of type Calculate 
// and can be passed into the UI, for example

// create the UI and run it
let form = new CalculatorUI.CalculatorForm(calculate)
form.Show()

let runWPFOnlyFsharp() = 
    let resource = new Uri("/Calculator;component/Calculator.fs.xaml",System.UriKind.Relative) 
    let runWindow = Application.LoadComponent(resource) :?> Window
    let txtHello:TextBlock = runWindow?txtHello
    let btnHello:Button = runWindow?btnHello
    do btnHello.Click.Add(fun _ -> do if txtHello.Text = "***" then txtHello.Text <- "Hello!" else txtHello.Text <- "***" ) 
    runWindow

[<STAThread>]
(new Application()).Run(runWPFOnlyFsharp()) |> ignore 