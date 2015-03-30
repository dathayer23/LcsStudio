namespace BamaLlama.XCS
open System
open System.Diagnostics
open System.IO
open Base
open Params
open Action
open TernaryCondition

module Environment = 
   type State = BinaryPattern

   [<AbstractClass>]
   [<AllowNullLiteral>]
   type Environment() = 
      
      let mutable currentConfiguration = 0
      let mutable currentState = 0
      
      
      member x.CurrentConfiguration with get() = currentConfiguration and set(v) = currentConfiguration <- v
      member x.CurrentState with get() = currentState and set(v) = currentState <- v
      member x.SubjectName = "EnvironmentBase"
      
      abstract member BeginExperiment : unit -> unit
      default x.BeginExperiment() = ()

      abstract member EndExperiment : unit -> unit
      default x.EndExperiment() = ()

      abstract member BeginProblem : bool -> unit
      default x.BeginProblem explore = ()

      abstract member EndProblem : unit -> unit
      default x.EndProblem() = ()

      abstract member Perform : Action -> unit
      abstract member Reward : unit -> double
      abstract member State : unit -> State
      abstract member Print : StreamWriter -> unit
      default x.Print fs = 
        fs.WriteLine(x.State().ToString())

      abstract member Stop : unit -> bool
      default x.Stop() = true
      abstract member Trace : StreamWriter -> unit
      abstract member ResetInput : unit -> unit
      abstract member NextInput : int -> bool
      abstract member RestoreState : StreamReader -> unit
      abstract member SaveState : StreamWriter -> unit
      abstract member SingleStep : unit -> bool
      default x.SingleStep() = true

      abstract member AllowTest : unit -> bool
      default x.AllowTest() = false
      
      abstract member ResetProblem : unit -> unit
      default x.ResetProblem() = x.ResetInput()

      abstract member NextProblem : unit -> bool
      default x.NextProblem() = false

    [<AllowNullLiteral>]
    type Multiplexer() =
       inherit Environment() 
       //let prms = paramDB.GetSubject("multiplexer")
       static let mutable classData = ClassData.NewClassData  "multiplexer_env"  "environment::multiplexer" null
       
       
       static let mutable addressSize = classData.parameters.TryGetInteger "address size" 3
       static let mutable layeredReward = classData.parameters.TryGetBool "layered reward" false
       static let mutable states = addressSize + (1 <<< addressSize)
       static let mutable numConfigurations = (1 <<< states)
       static let mutable stateSize = addressSize + (int)(2.0 ** (double)addressSize)
       
       let mutable inputs : State = new BinaryPattern(states)
       let mutable firstProblem = true
       let mutable currentReward = 0.0
       let mutable solved = false
       let mutable targetIndex = 0
       

       /// take binary string and convert to long
       let b2long (str:string) = 
          if String.IsNullOrEmpty(str) then 0
          else
             let chars = Array.toList (str.ToCharArray())
             let rec _b2long acc chs = 
                match chs with 
                | [] -> acc
                | c :: cs -> 
                   match c with 
                   | '0' -> _b2long (acc * 2) cs
                   | '1' -> _b2long (acc * 2 + 1) cs
             _b2long 0 chars

       static member NewMultiplexer(``params``) =
          let env = new Multiplexer()
          do Multiplexer.Init(``params``)
          env 

       static member private Init(paramDb) =
          do classData <- ClassData.NewClassData "multiplexer_env"  "environment::multiplexer" paramDb
          do addressSize <- classData.parameters.TryGetInteger "address size" 3
          do layeredReward <- classData.parameters.TryGetBool "layered reward" false
          do states <- addressSize + (1 <<< addressSize)
          do numConfigurations <- (1 <<< states)
          do stateSize <- addressSize + (int)(2.0 ** (double)addressSize)


       override x.NextProblem() = x.NextInput numConfigurations

       override x.State() = new BinaryPattern(Utility.long2binary base.CurrentState stateSize) 
       override x.BeginProblem(explore) = 
          do currentReward <- 0.0
          do inputs <- new BinaryPattern(states)
          do targetIndex <- b2long(inputs.ToString().Substring(0,addressSize))
          do firstProblem <- false
          ()

       override x.Perform(action) = 
          let address = addressSize + targetIndex
          if not layeredReward
          then
             if inputs.[address].Value = action.Value
             then
                do currentReward <- 1000.0
                do solved <- true
             else
                do currentReward <- 0.0
                do solved <- false
          else
             if inputs.[address].Value = action.Value
             then
                do currentReward <- (float)(300 + targetIndex * 200 + inputs.[address].Value * 100)
                do solved <- true
             else
                do currentReward <- (float)(targetIndex * 200 + inputs.[address].Value * 100)
                do solved <- false

       override x.Trace sw = 
          if solved 
          then sw.Write(sprintf "%d" 1) 
          else sw.Write(sprintf "%d" 0)

       override x.ResetInput() = 
          do base.CurrentState <- 0
          do inputs <- new BinaryPattern(Utility.long2binary base.CurrentState stateSize)   

       override x.NextInput numConfigurations = 
          do base.CurrentState <- base.CurrentState + 1
          if base.CurrentState < numConfigurations
          then
             do inputs <- new BinaryPattern(Utility.long2binary base.CurrentState  stateSize)   
             true
          else
             do base.CurrentState <- 0
             do inputs <- new BinaryPattern(Utility.long2binary base.CurrentState  stateSize)   
             false
       
       override x.SaveState sw = sw.WriteLine (sprintf "\n%d\n" base.CurrentState)
       
       override x.Reward() = currentReward
       
       override x.RestoreState sr = 
          do base.CurrentState <- Utility.intFromStrWithDefault (sr.ReadLine().Trim()) 0
          x.BeginProblem(true)