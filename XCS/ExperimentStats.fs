namespace BamaLlama.XCS
open System
open System.Diagnostics
open System.IO

module ExperimentStats = 
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
         currentTestProblem : int
         compactAvgSteps : double
         compactAvgRewardSum : double
         compactAverageSize : double
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
           currentTestProblem = 0
           compactAvgSteps = 0.0
           compactAvgRewardSum = 0.0
           compactAverageSize = 0.0
         }

      member x.StartExperiment() =
         do x.experimentTimer.Start()
         {
            x with avgProblemTime = 0.0; compactStatsPrinted = false; currentTestProblem = 0
                   compactAvgSteps = 0.0; compactAvgRewardSum = 0.0; compactAverageSize = 0.0
         }


