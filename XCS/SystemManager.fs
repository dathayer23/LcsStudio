namespace BamaLlama.XCS

open System
open System.Diagnostics
open System.IO
open Microsoft.FSharp.Core.Operators
open Base
open Utility
open Params
open ClassifierSystem
open Environment
open ExperimentStats

module SystemManager = 
   let CreateEnvironment str pms = 
      match str with
      | "parity" -> Multiplexer.NewMultiplexer(pms)
         
      | _ -> failwith (sprintf "Unknown Environement specified '%s'" str)

   [<AllowNullLiteral>]
   type LcsManager(``params``: ParameterDB) =     
      let (classData:ClassData) = ClassData.NewClassData "experiment_manager" "experiments"  ``params``
      let expParams = classData.parameters
      let firstExperiment = expParams.TryGetInteger "first experiment" 0
      let numExperiments = expParams.TryGetInteger "number of experiments" 0
      let firstLearningProblem = expParams.TryGetInteger "first problem" 0
      let numLearningProblems = expParams.TryGetInteger "number of learning problems" 0
      let numCondensationProblems = expParams.TryGetInteger "number of condensation problems" 0
      let numTestProblems = expParams.TryGetInteger "number of test problems" 0
      let maxSteps = expParams.TryGetInteger "maximum steps" 100
      let doTrace = expParams.TryGetBool "do trace" true
      let doTestEnvironment = expParams.TryGetBool "do test environment" true
      let saveInterval = expParams.TryGetInteger "save state every" 10
      let mutable trace = expParams.TryGetBool "trace experiments" true
      let testEnvironment = expParams.TryGetBool "test environment" true
      let saveState = expParams.TryGetBool "save experiment state" true
      let saveAgentState = expParams.TryGetBool "save population state" true
      let saveAgentReport = expParams.TryGetBool "save population report" true
      let saveTraceTime = expParams.TryGetBool "trace time" true
      let fileExtension = expParams.TryGetString "file extension" ""
      let compactMode = expParams.TryGetBool "compact mode" false
      let saveStatsEvery = expParams.TryGetInteger "save statistics every" 10
      let compactAvgSteps = expParams.TryGetInteger "compact average steps" 10
      let compactAvgRewardSum = expParams.TryGetDouble "compact average reward sum" 0.0
      let compactAvgSize = expParams.TryGetInteger "compact average size" 0
      let bufferedOutput = expParams.TryGetBool "buffered output" true
      let environment = CreateEnvironment (expParams.TryGetString "environment" "parity") ``params``
     
      do if compactMode then trace <- false
      let mutable currentExperiment = -1
      let mutable currentPropblem = -1
      let mutable currentNumTestProblems = 0
      let mutable classifierSystem : ClassifierSystem = ClassifierSystem.NewClassifierSystem(``params``,0,0)
      
      let saveAgentReport expNo problemNo = ()
      let saveAgentState  expNo problemNo = ()
      let restoreAgent expNo = ()
      let mutable statisticsFile = null
      let mutable traceFile = null     

      member x.PerformExperiments() = 
         
         let overallTimer = new Stopwatch()
         let experimentTimer = new Stopwatch()
         let problemTimer = new Stopwatch()

         let expStats : ExperimentStats ref = ref (ExperimentStats.newExperimentStats())

         let statsFileName currentExperiment = 
            sprintf "%s.%s-%d" (if compactMode then "compact_stats" else "statistics") 
               fileExtension currentExperiment
         
         let traceFileName currentExperiment = 
            sprintf "trace.%s-%d" fileExtension currentExperiment

         let explorationFlag = false
         let condensationFlag = false
         let openFiles currExperiment = 
            let stats = statsFileName currentExperiment
            let trace = traceFileName currentExperiment
            Console.WriteLine("Opening Statistics File {0}", stats)
            do statisticsFile <- new StreamWriter(stats)
            Console.WriteLine("Opening Trace File {0}", trace)
            do traceFile <- new StreamWriter(trace)
               
         let ExecuteProblemStep() = 
            do classifierSystem.Step(explorationFlag, condensationFlag)
            do expStats :=  (!expStats).EndProblemStep (environment.Reward())
             
         
         let ExecuteProblemSteps() = 
            let mutable run = true
            while run do
              ExecuteProblemStep()
              if (!expStats).problemSteps > maxSteps || environment.Stop() 
              then run <- false



         let performProblem (currProblem:int)  = 
            Console.WriteLine("Performing Problem {0}", currProblem)
            let fileStats() = ()

            do fileStats()
            do classifierSystem.BeginProblem()
            let condensationFlag = 
               numCondensationProblems > 0 &&
               (currProblem >= firstLearningProblem + 2 * numLearningProblems)
            do environment.BeginProblem(explorationFlag)
            do expStats :=  (!expStats).StartProblem()
            do ExecuteProblemSteps()
            do expStats :=  (!expStats).EndProblem()


         let performProblems() = 
            let lastProblem = 
               firstLearningProblem + 2 * 
                        (numLearningProblems + numCondensationProblems) + numTestProblems
            Console.WriteLine("First Problem is {0} Last problem is {1}", firstLearningProblem, lastProblem)
            [firstLearningProblem .. lastProblem] |> List.iter (fun i -> performProblem i)

         let performExperiment (currExperiment:int) = 
            Console.WriteLine("Performing Experiment {0}", currExperiment)
            do openFiles currExperiment
            do expStats :=  (!expStats).StartExperiment()
            do classifierSystem.BeginExperiment(environment)
            do performProblems()
         
         do experimentTimer.Reset()
         do problemTimer.Reset()
         do overallTimer.Reset()

         [firstExperiment .. (firstExperiment + numExperiments)]
            |> List.iter (fun i -> performExperiment(i))


      member x.PrintParams() =
         do Console.WriteLine("first experiment : {0}", firstExperiment)
         do Console.WriteLine("number of experiments : {0}", numExperiments)
         do Console.WriteLine("first learning problem : {0}", firstLearningProblem)
         do Console.WriteLine("number of learning problems : {0}", numLearningProblems)
         do Console.WriteLine("number of condensation problems : {0}", numCondensationProblems)
         do Console.WriteLine("Number Of Test Problemst : {0}", numTestProblems)
         do Console.WriteLine("maximum steps : {0}", maxSteps)
         do Console.WriteLine("do trace : {0}", doTrace)
         do Console.WriteLine("do test environment : {0}", doTestEnvironment)


      member x.PrintSaveOptions(file:FileStream) = ()
      member x.SaveState() = 0
      member x.RestoreState(experimentNumber:Int64) = ()

