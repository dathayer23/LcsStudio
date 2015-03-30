namespace BamaLlama.XCS

open System
open System.IO
open Microsoft.FSharp.Core.Operators
open Base
open Utility
open Params
open Interfaces
open Action
open ConditionBase
open TernaryCondition
open Classifier
open Environment

module ClassifierSystem = 
   type SystemPrediction = 
      {
         payoff : double;
         action : IAction<int>;
         sum : double;
         n : Int64;
      }

   type ClassifierStatistics() = 
      let mutable avgPrediction = 0.0
      let mutable avgFitness = 0.0
      let mutable avgError = 0.0
      let mutable avgActionSetSize = 0.0
      let mutable avgExperience = 0.0
      let mutable avgNumerosity = 0.0
      let mutable avgTimeStamp = 0.0
      let mutable avgNoUpdates = 0.0
      let mutable systemError = 0.0
      let mutable numMacroClassifiers = 0
      let mutable numGAs = 0
      let mutable numCovers = 0
      let mutable numSubsumptions = 0

      member x.SystemError with get() = systemError and set(v) = systemError <- v
      member x.NumGAs with get() = numGAs and set(v) = numGAs <- v
      member x.IncNumGas() = x.NumGAs <- x.NumGAs + 1
      member x.Reset() = 
         do avgPrediction <- 0.0
         do avgFitness <- 0.0
         do avgError <- 0.0
         do avgActionSetSize <- 0.0
         do avgExperience <- 0.0
         do avgNumerosity <- 0.0
         do avgTimeStamp <- 0.0
         do avgNoUpdates <- 0.0
         do systemError <- 0.0
         do numMacroClassifiers <- 0
         do numGAs <- 0
         do numCovers <- 0
         do  numSubsumptions <- 0
         x

      member x.ToCsvString() : string =
        sprintf  "%f,%f,%f,%f,%f,%f,%f,%f,%f,%d,%d,%d,%d" 
           avgPrediction avgFitness avgError avgActionSetSize 
           avgExperience avgNumerosity avgTimeStamp avgNoUpdates 
           systemError numMacroClassifiers numGAs numCovers numSubsumptions

      member x.FromCsvString (str:string) = 
         let fields : string array = str.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
         do assert(fields.Length = 13)
         do avgPrediction <- dblFromStrWithDefault (fields.[0]) 0.0
         do avgFitness <- dblFromStrWithDefault (fields.[1]) 0.0
         do avgError <- dblFromStrWithDefault (fields.[2]) 0.0
         do avgActionSetSize <- dblFromStrWithDefault (fields.[3]) 0.0
         do avgExperience <- dblFromStrWithDefault (fields.[4]) 0.0
         do avgNumerosity <- dblFromStrWithDefault (fields.[5]) 0.0
         do avgTimeStamp <- dblFromStrWithDefault (fields.[6]) 0.0
         do avgNoUpdates <- dblFromStrWithDefault (fields.[7]) 0.0
         do systemError <- dblFromStrWithDefault (fields.[8]) 0.0
         do numMacroClassifiers <- intFromStrWithDefault (fields.[9]) 0
         do numGAs <- intFromStrWithDefault (fields.[10]) 0
         do numCovers <- intFromStrWithDefault (fields.[11]) 0
         do  numSubsumptions <- intFromStrWithDefault (fields.[12]) 0
         ()
   
   type ClassifierSystem(size:int, width:int) =
      // Static class data
      static let mutable (classData:ClassData) = ClassData.Empty //ClassData.NewClassData "xcs_classifier_system" "classifier_system" paramDB
      static let mutable clsParams = new Parameters() //classData.parameters
      static let mutable expParams = ExperimentParameters.FromParameters(clsParams)
      static let mutable popParams = PopulationParameters.FromParameters(clsParams)
      static let mutable classifierParams =  ClassifierParameters.FromParameters(clsParams)
      static let mutable strategyParams = StrategyParameters.FromParameters(clsParams)
      static let mutable gaParams = GaParameters.FromParameters(clsParams)
      static let mutable deletionParams = DeletionParameters.FromParameters(clsParams)
      static let mutable initialized = false

      //instance Data
      let mutable classifiers : classifier array = [||]
      let mutable statistics  = new ClassifierStatistics()      
      let mutable matchSet = [||] 
      let mutable actionSet = [||]
      let mutable prevActionSet = [||]
      let mutable predictionArray : SystemPrediction[] = [||]
      let mutable (env:Environment) = null
      let mutable (prevInput:BinaryPattern) = BinaryPattern.Zero(1)
      let mutable prevReward = 0.0
      let createCover (matchSet :classifier array) = 
         if matchSet.Length = 0 then true
         else
            let avgPopPrediction = Array.averageBy (fun (cl:classifier) -> cl.TotalPrediction) classifiers
            let sumMatchPrediction = Array.sumBy (fun (cl:classifier) -> cl.TotalPrediction) matchSet
            sumMatchPrediction <= strategyParams.fractionForCovering * avgPopPrediction

      let initClassifier (cl:classifier) = cl
      let insertClassifier cl = ()

      let initRandomPopulation() = 
         do [1 .. popParams.maxPopulation ]
                |> List.map (
                     fun _ -> new classifier(classifierParams.classifierWidth, expParams.maxAction)
                                  |> initClassifier 
                                  |> insertClassifier) |> ignore
         do popParams.macroSize <- classifiers.Length
         do popParams.populationSize <- popParams.maxPopulation   
         ()

      let initPopulationLoad fileName = [||]
         
      let initClassifierSet() = 
         match popParams.populationInit with 
         | PopInitStrategy.Empty  -> classifiers <- [||]
         | PopInitStrategy.Random -> initRandomPopulation()
         | PopInitStrategy.Load   -> classifiers <- initPopulationLoad popParams.populationInitFile

      let deleteClassifier() = ()

      let performStandardCovering matchSet pattern =  
         if createCover(matchSet)
         then
            let newClassifier = classifier.Cover( pattern )
            let newClassifier = initClassifier newClassifier
            do insertClassifier newClassifier
            do deleteClassifier()
            true
         else false

      let performNmaCovering matchSet pattern = false

      static member NewClassifierSystem(``params``:ParameterDB, size, width) = 
         let css = new ClassifierSystem(size,width)
         do ClassifierSystem.Init(``params``)
         css

      static member private Init (``params``:ParameterDB) = 
         do classData <- ClassData.NewClassData "xcs_classifier_system" "classifier_system" ``params``
         do clsParams <- classData.parameters

         do expParams <- ExperimentParameters.FromParameters clsParams
         do popParams <- PopulationParameters.FromParameters clsParams
         do classifierParams <- ClassifierParameters.FromParameters clsParams
         do strategyParams <- StrategyParameters.FromParameters clsParams
         do gaParams <- GaParameters.FromParameters  clsParams
         do deletionParams <- DeletionParameters.FromParameters clsParams
         do initialized <- true

      member x.TotalSteps with get() = expParams.totalSteps and set(v) = expParams.totalSteps <- v
      member x.TotalLearningSteps with get() = expParams.totalLearningSteps and set(v) = expParams.totalLearningSteps <- v
      member x.ProblemSteps with get() = expParams.problemSteps and set(v) = expParams.problemSteps <- v
      member x.TotalReward with get() = expParams.totalReward and set(v) = expParams.totalReward <- v

      member x.PopulationSize with get() = popParams.populationSize and set(v) = popParams.populationSize <- v
      member x.MacroSize with get() = popParams.macroSize and set(v) = popParams.macroSize <- v
      
      member x.Width = classifierParams.classifierWidth
      member x.PerformCovering(matchSet :classifier array, pattern : BinaryPattern) = 
         match strategyParams.coveringStrategy with
         | CoveringStrategy.Standard -> performStandardCovering matchSet pattern
         | CoveringStrategy.ActionBased -> performNmaCovering matchSet pattern
         
         
      
      member x.MatchSet(pattern : BinaryPattern) = 
         do matchSet <- Array.filter (fun (cl:classifier) -> cl.Match(pattern)) classifiers
         matchSet.Length

      member x.ActionSet(a:Action) = 
         do actionSet <- Array.filter (fun (cl:classifier) -> cl.Action = a) matchSet
         actionSet.Length

      member x.BuildPredictionArray() = ()
      member x.SelectAction str = new Action(1)
      member x.BeginExperiment(e) = 
         do env <- e
         do x.TotalSteps <- 0
         do x.TotalLearningSteps <- 0
         do statistics <- statistics.Reset()
         do x.PopulationSize <- 0
         do x.MacroSize <- 0
         initClassifierSet()

      member x.BeginProblem() = 
         do prevActionSet <- [||]
         do actionSet <- [||]
         do x.ProblemSteps <- 0
         x.TotalReward <- 0.0
      
      member x.EndProblem() =
         do matchSet <- [||] 
         actionSet <- [||]

      member x.ReinforcementLearning() = ()
      member x.NeedGa (actionSet, explorationMode) = false
      member x.GeneticAlgorithm actionSet prevInput condensationMode = ()
      member x.UpdateSet reward set = ()
      member x.Step (exploreMode, condensationMode) = 
         let currInput = new BinaryPattern(env.CurrentState, x.Width)
         if exploreMode then x.TotalSteps <- x.TotalSteps + 1
         let mutable performCovering = true
         while(performCovering) do
            let matchSetSize = x.MatchSet currInput
            performCovering <- x.PerformCovering(matchSet, currInput)

         do x.BuildPredictionArray()
         let action = 
            if exploreMode then x.SelectAction(strategyParams.actionSelectionStrategy)
            else  x.SelectAction(ActionSelectionStrategy.Deterministic)

         let size =  x.ActionSet(action)
         do prevInput <- currInput
         do env.Perform(action)
         if env.SingleStep()
         then 
            let payoff = predictionArray.[action.Value].payoff
            do statistics.SystemError <- Math.Abs(payoff - env.Reward())

         do expParams.totalReward <- expParams.totalReward + env.Reward()

         if (exploreMode || gaParams.flagUpdateTest) && prevActionSet.Length > 0
         then 
            do x.ReinforcementLearning()
         
         if env.Stop()
         then  x.UpdateSet (env.Reward()) actionSet
          
         if (gaParams.flagDiscoveryComponent && x.NeedGa(actionSet, exploreMode))
         then 
            do x.GeneticAlgorithm actionSet prevInput condensationMode
            statistics.IncNumGas()
            
         prevActionSet <- actionSet
         actionSet <- [||] 
         prevReward <- env.Reward()
                              