namespace BamaLlama.XCS

open System
open System.Diagnostics
open System.IO
open Microsoft.FSharp.Core.Operators
open Base
open Utility
open Params

module SystemManager = 
   type ExperimentStats = 
      {
         rewardSum : double
         problemSteps :int
         totalTimer : Stopwatch
         experimentTimer : Stopwatch
         problemTimer : Stopwatch
         experimentTimes : double list
         problemTimes : double list
         avgProblemTime : double
         avgLearningTime : double
         avgTestingTime : double
         compactStatsPrinted : bool
      }
   with 
      static member newExperimentStats() = 
         {
           rewardSum = 0.0; problemSteps = 0; 
           totalTimer = new Stopwatch()
           experimentTimer = new Stopwatch()
           problemTimer = new Stopwatch()
           experimentTimes = []; problemTimes = []
           avgProblemTime = 0.0; avgLearningTime = 0.0
           avgTestingTime = 0.0; compactStatsPrinted = false
         }


   type LcsManager() = 
      let statisticsFile = new File("Statistics.Txt")
      let traceFile = new File("Trace.txt")
      let stats = ExperimentStats.newExperimentStats()

      member x.PerformExperiments() = ()