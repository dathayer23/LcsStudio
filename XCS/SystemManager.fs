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
   
   type LcsManager(``params``: ParameterDB) =      
      let expParams = ``params``.GetSubject("experiments")
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

      do if compactMode then trace <- false
      let mutable currentExperiment = -1
      let mutable currentPropblem = -1
      let mutable currentNumTestProblems = 0
      let mutable classifierSystem : ClassifierSystem = new ClassifierSystem(0,0,``params``)
      let saveAgentReport expNo problemNo = ()
      let saveAgentState  expNo problemNo = ()
      let restoreAgent expNo = ()

      member x.PerformExperiments() = 
         let mutable statisticsFile = null
         let mutable traceFile = null     
         let expStats : ExperimentStats ref = ref (ExperimentStats.newExperimentStats())

         let statsFileName currentExperiment = 
            sprintf "%s.%s-%d" (if compactMode then "compact_stats" else "statistics") 
               fileExtension currentExperiment
         
         let traceFileName currentExperiment = 
            sprintf "trace.%s-%d" fileExtension currentExperiment

         let explorationFlag = false
         let condensationFlag = false
         let openFiles currExperiment = ()
         let ExecuteProblemStep() = 
            do classifierSystem.Step(explorationFlag, condensationFlag)
            do expStats :=  (!expStats).IncProblemSteps()
         let ExecuteProblemSteps() = ()

         let performProblem currProblem = 
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


         let performProblems() = ()

         let performExperiment currExperiment = 
            do openFiles currExperiment
            do expStats :=  (!expStats).StartExperiment()
            do classifierSystem.BeginExperiment()
            do performProblems()
         ()


      member x.PrintSaveOptions(file:FileStream) = ()
      member x.SaveState() = 0
      member x.RestoreState(experimentNumber:Int64) = ()

