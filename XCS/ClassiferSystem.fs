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

module ClassiferSystem = 
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
   type DiscoveryStrategy = GA | Roulette | None
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
       
   let SelectPopInitStrategy str = 
      match str with 
      | "random" ->  PopInitStrategy.Random
      | "empty" ->  PopInitStrategy.Empty
      | "load" -> PopInitStrategy.Load
      | _ -> PopInitStrategy.Empty

   let SelectActionStrategy str = ActionSelectionStrategy.Deterministic
   let SelectDiscoveyComponent str = DiscoveryStrategy.None
   let SelectDeletionStrategy str = DeletionStrategy.Random

   type ClassifierSystem(size, width, p:ParameterDB) =
      let parameters: Parameters = p.GetSubject("ClassifierSystem")

      let classifiers : classifier array = [||]
      let statistics  = new ClassifierStatistics()
      
      let mutable matchSet = [||] 
      let mutable actionSet = [||]

      // experiment parameters
      let totalSteps = parameters.TryGetInteger " " 0
      let totalLearningSteps = parameters.TryGetInteger " " 0
      let totalTime = parameters.TryGetInteger " " 0
      let problemSteps = parameters.TryGetInteger " "  0
      let totalRewaed = parameters.TryGetDouble "" 0.0
      let systemError = parameters.TryGetDouble "" 0.0
      let maxAction = parameters.TryGetInteger "" 8
      // Population parameters
      let maxPopulation = parameters.TryGetInteger "population size" 100
      let populationSize = parameters.TryGetInteger " " 0
      let macroSize = parameters.TryGetInteger " " 0
      let populationInit : PopInitStrategy = SelectPopInitStrategy (parameters.TryGetString " " "default")
      let populationInitFile = ""

      // Classifier parameters 
      let initPrediction = parameters.TryGetDouble "" 0.0
      let initError = parameters.TryGetDouble "" 0.0
      let initFitness = parameters.TryGetDouble "" 0.0
      let initSetSize = parameters.TryGetDouble "" 0.0
      let initNumUpdates = parameters.TryGetInteger " " 0

      //covering strategy parameters
      let coveringStrategy = SelectCoverStrategy (parameters.TryGetString "covering strategy" "standard")
      let tethaNma = parameters.TryGetDouble "covering threshold" 0.0
      let fractionForCovering = parameters.TryGetDouble "" 0.0

      //action selection parameters
      let actionSelectionStrategy : ActionSelectionStrategy = SelectActionStrategy (parameters.TryGetString " " "default")

      let propRandomAction = parameters.TryGetDouble "" 0.0

      //fitness computation parameters
      let epsilonZero = parameters.TryGetDouble ""  0.0
      let alpha = parameters.TryGetDouble "" 0.0
      let vi = parameters.TryGetDouble "" 0.0
      let useExponentialFitness = parameters.TryGetBool " " false

      // ga parameters
      let discoveryComponent = SelectDiscoveyComponent(parameters.TryGetString "discovery component" "default")
      let flagDiscoveryComponent = parameters.TryGetBool " " true;
      let thetaGA = parameters.TryGetDouble "" 0.0
      let propCrossover = parameters.TryGetDouble "" 0.0
      let probMutation = parameters.TryGetDouble "" 0.0
      let flagGaAvgInit = parameters.TryGetBool " " false
      let flagErrorUpdateFirst = parameters.TryGetBool " " false
      let flagUpdateTest = parameters.TryGetBool " " false
      let useGa = parameters.TryGetBool " " true
      let useCrossover = parameters.TryGetBool " " true
      let useMutation = parameters.TryGetBool " " true

      //subsumption deletion parameters
      let flagGaSubsumption = parameters.TryGetBool " " true
      let flagGaaSubsumption = parameters.TryGetBool " " false
      let flagAsSubsumption = parameters.TryGetBool " " true
      let thetaSub = parameters.TryGetDouble "" 0.0
      let thetaAsSub = parameters.TryGetDouble "" 0.0
      let flagCoverAvgInit = parameters.TryGetBool " " true

      //delete parameters
      let deletionStrategy : DeletionStrategy = SelectDeletionStrategy (parameters.TryGetString " " "default")
      let flagDeleteWithAccuracy = parameters.TryGetBool " " false
      let thetaDel = parameters.TryGetDouble "" 0.0
      let deltaDel = parameters.TryGetDouble "" 0.0

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
      let deleteClassifier() = ()

      let performStandardCovering matchSet pattern =  
         if createCover(matchSet)
         then
            let newClassifier = classifier.Cover pattern  (new Action(dice maxAction)) parameters
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
         | _ -> performStandardCovering matchSet pattern
         
      
      member x.MatchSet(pattern : BinaryPattern) = 
         do matchSet <- Array.filter (fun (cl:classifier) -> cl.Match(pattern)) classifiers
         matchSet

      member x.ActionSet(a:Action) = 
         do actionSet <- Array.filter (fun (cl:classifier) -> cl.Action = a) matchSet
         actionSet