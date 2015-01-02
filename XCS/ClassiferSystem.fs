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
   
   type CoveringStrategy = Standard | ActionBased
   type PopInitStrategy = Random | Empty | Load
   type DiscoveryStrategy = GA | Roulette | NoDiscovery
   type DeletionStrategy = 
        RwsSetbased
      | RwsFitness
      | Random
      | RandomWithAccuracy
      | Tses
      | Tse
      | Tsf
      | Tsfs
      | Tsss

   type ActionSelectionStrategy = 
        Deterministic 
      | Uniform 
      | SemiUniform 
      | Proportional

   type SelectionStrategy = 
       RwsFitness    //select offspring classifiers based on RWS and fitness
     | TsFitness     //select offspring based on TS and fitness maximization
     | TsError       //select offspring based on TS and error minimization

   let SelectCoverStrategy str = 
      match str with 
      | "standard" -> CoveringStrategy.Standard
      | "action based" -> CoveringStrategy.ActionBased
      | _ ->   CoveringStrategy.Standard
       
   let SelectPopInitStrategy (str:string) = 
      match str.ToLower() with 
      | "random" ->  PopInitStrategy.Random
      | "empty" ->  PopInitStrategy.Empty
      | "load" -> PopInitStrategy.Load
      | _ -> PopInitStrategy.Empty

   let SelectActionStrategy (str:string) : ActionSelectionStrategy * double option = 
      match str.ToLower() with 
      | "deterministic" -> (ActionSelectionStrategy.Deterministic, None)
      | "proportional" -> (ActionSelectionStrategy.Proportional, None)
      | "random" -> (ActionSelectionStrategy.SemiUniform, Some( 1.0))
      | _ when str.StartsWith("semiuniform") -> 
          let prb = dblFromStrWithDefault (str.Substring(12)) 0.5
          if prb <= 0.0 || prb > 1.0 
          then  failwith (sprintf "'Biased' parameter (%f) out of range (0.0,1.0]" prb)

          (ActionSelectionStrategy.SemiUniform, Some( prb))
      | _ -> failwith (sprintf "Unrecognized Exploration Policy '%s'" str)


   let SelectDiscoveyComponent str = DiscoveryStrategy.GA
   let SelectDeletionStrategy (str:string) = 
      match str.ToLower() with 
      | "standard" -> DeletionStrategy.RwsSetbased, false
      | "accuracy-based" -> DeletionStrategy.RwsFitness, true
      | "random-with-accuracy" -> DeletionStrategy.RandomWithAccuracy, true
      | "random" -> DeletionStrategy.Random, false
      | _ -> failwith (sprintf "Unrecognized Deletion Policy '%s'" str)
       

   type ClassifierSystem(size:int, width:int) =
      static let classData = ClassData.NewClassData "xcs_classifier_system" "classifier_system"

      let parameters: Parameters = classData.parameters

      let mutable classifiers : classifier array = [||]
      let mutable statistics  = new ClassifierStatistics()
      
      let mutable matchSet = [||] 
      let mutable actionSet = [||]

      // experiment parameters
      let totalSteps = parameters.TryGetInteger "total experiment steps" 0
      let totalLearningSteps = parameters.TryGetInteger "total learning steps" 0
      let totalTime = parameters.TryGetInteger "total time" 0
      let problemSteps = parameters.TryGetInteger "problem steps"  0
      let totalReward = parameters.TryGetDouble "total reward" 0.0
      let systemError = parameters.TryGetDouble "system error" 0.0
      let maxAction = parameters.TryGetInteger "max action" 8

      // Population parameters
      let maxPopulation = parameters.TryGetInteger "max population size" 100
      let mutable populationSize =  0
      let mutable macroSize =  0
      let populationInit : PopInitStrategy = 
         SelectPopInitStrategy (parameters.TryGetString "population init" "default")
      let populationInitFile = parameters.TryGetString "population init file" "classifiers.txt"

      // Classifier parameters 
      let initPrediction = parameters.TryGetDouble "prediction init" 0.0
      let initError = parameters.TryGetDouble "error init" 0.0
      let initFitness = parameters.TryGetDouble "fitness init" 0.0
      let initSetSize = parameters.TryGetDouble "set size init" 0.0
      let initNumUpdates = parameters.TryGetInteger "number updates init" 0
      let classifierWidth = parameters.TryGetInteger "classifier width" 24

      //covering strategy parameters
      let coveringStrategy = SelectCoverStrategy (parameters.TryGetString "covering strategy" "standard")
      let tethaNma = parameters.TryGetDouble "covering threshold" 0.0
      let fractionForCovering = parameters.TryGetDouble "fraction for covering" 0.0

      //action selection parameters
      let actionSelectionStrategy, prbRnd = SelectActionStrategy (parameters.TryGetString " " "default")
      let explorationStrategy = parameters.TryGetString "exploration strategy" "greedy"
      let probRandomAction = parameters.TryGetDouble "prob random action" (match prbRnd with | Some f -> f | None -> 0.0)
                                                                           
      //fitness computation parameters
      let epsilonZero = parameters.TryGetDouble "epsilon zero"  0.0
      let alpha = parameters.TryGetDouble "alpha" 0.0
      let vi = parameters.TryGetDouble "vi" 0.0
      let useExponentialFitness = parameters.TryGetBool "use exponential fitness" false

      // ga parameters
      let discoveryComponent = SelectDiscoveyComponent(parameters.TryGetString "discovery component" "default")
      let flagDiscoveryComponent = parameters.TryGetBool " " true;
      let thetaGA = parameters.TryGetDouble "theta GA" 0.0
      let propCrossover = parameters.TryGetDouble "crossover probability" 0.0
      let probMutation = parameters.TryGetDouble "mutation probability" 0.0
      let flagGaAvgInit = parameters.TryGetBool " " false
      let flagErrorUpdateFirst = parameters.TryGetBool "update error first" false
      let flagUpdateTest = parameters.TryGetBool "update during test" false
      let useGa = parameters.TryGetBool "use GA" true
      let useCrossover = parameters.TryGetBool "use crossover" true
      let useMutation = parameters.TryGetBool "use mutation" true

      //subsumption deletion parameters
      let flagGaSubsumption = parameters.TryGetBool "GA Subsumption" true
      let flagGaaSubsumption = parameters.TryGetBool "GAA Subsumption" false
      let flagAsSubsumption = parameters.TryGetBool "AS Subsimption" true
      let thetaGaSub = parameters.TryGetDouble "theta GA sub" 0.0
      let thetaAsSub = parameters.TryGetDouble "theta As sub" 0.0
      let flagCoverAvgInit = parameters.TryGetBool " " true

      //delete parameters
      let deletionStrategy, flagDeleteWithAccuracy = 
         SelectDeletionStrategy (parameters.TryGetString "deletion strategy" "default")
      // allow parameters to override default setting
      let flagDeleteWithAccuracy = parameters.TryGetBool "delete with accuracy" false
      let thetaDel = parameters.TryGetDouble "theta delete" 0.0
      let deltaDel = parameters.TryGetDouble "delta delete" 0.0

      // reinforcement parameters
      let learningRate = parameters.TryGetDouble "learning rate" 0.8
      let discountFactor = parameters.TryGetDouble "discount factor" 0.2

      let createCover (matchSet :classifier array) = 
         if matchSet.Length = 0 then true
         else
            let avgPopPrediction = Array.averageBy (fun (cl:classifier) -> cl.TotalPrediction) classifiers
            let sumMatchPrediction = Array.sumBy (fun (cl:classifier) -> cl.TotalPrediction) matchSet
            sumMatchPrediction <= fractionForCovering * avgPopPrediction

      let initClassifier (cl:classifier) = cl
      let insertClassifier cl = ()

      let initRandomPopulation() = 
         do [1 .. maxPopulation ]
                |> List.map (
                     fun _ -> new classifier(width, maxAction)
                                  |> initClassifier 
                                  |> insertClassifier) |> ignore
         do macroSize <- classifiers.Length
         do populationSize <- maxPopulation   
         ()

      let initPopulationLoad fileName = [||]
         
      let initClassifierSet() = 
         match populationInit with 
         | PopInitStrategy.Empty  -> classifiers <- [||]
         | PopInitStrategy.Random -> initRandomPopulation()
         | PopInitStrategy.Load   -> classifiers <- initPopulationLoad populationInitFile


      
      
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

      member x.PerformCovering(matchSet :classifier array, pattern : BinaryPattern) = 
         match coveringStrategy with
         | CoveringStrategy.Standard -> performStandardCovering matchSet pattern
         | CoveringStrategy.ActionBased -> performNmaCovering matchSet pattern
         
         
      
      member x.MatchSet(pattern : BinaryPattern) = 
         do matchSet <- Array.filter (fun (cl:classifier) -> cl.Match(pattern)) classifiers
         matchSet

      member x.ActionSet(a:Action) = 
         do actionSet <- Array.filter (fun (cl:classifier) -> cl.Action = a) matchSet
         actionSet

      member x.BeginExperiment() = ()
      member x.BeginProblem() = ()
      member x.Step (explore, condense) = ()