namespace BamaLlama.XCS
open Params
module Base = 
   type ClassData = { className : string; tagName : string ; mutable parameters : Parameters }
   with   
      member x.SetParameters (pms: ParameterDB) =  x.parameters <- pms.GetSubject(x.tagName)
      member x.ClassName : string = x.className
      member x.TagName : string = x.tagName
      static member Empty =  ClassData.NewClassData "" "" null
      static member NewClassData className tagName (paramDB: ParameterDB) = 
         { 
            className = className; 
            tagName = tagName; 
            parameters = match paramDB with | null -> Parameters() | _ ->  paramDB.GetSubject(tagName)
         }


   type ParameterizedClass(className, tagName, paramDB) = 
      let mutable classData = ClassData.NewClassData className tagName paramDB
      member x.SetParameters (prms: ParameterDB) = classData.SetParameters prms
      member x.SetClassData className  tagName paramDB  = do classData <- ClassData.NewClassData className tagName paramDB
      member x.ClassName = classData.ClassName
      member x.TagName = classData.TagName
      member x.Parameters with get() =  classData.parameters and set(v) = classData.parameters <- v

   type ExplorationStrategy = Greedy | Explore | Random | Probing
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
      | _ ->  (ActionSelectionStrategy.Proportional, None)

   let SelectExplorationStrategy (str:string) : ExplorationStrategy = 
      match str.ToLower() with 
      | "greedy" -> ExplorationStrategy.Greedy
      | "explore" -> ExplorationStrategy.Explore
      | "random" -> ExplorationStrategy.Random
      | "probing" -> ExplorationStrategy.Probing
      | _ -> ExplorationStrategy.Random

   let SelectDiscoveyComponent str = DiscoveryStrategy.GA
   let SelectDeletionStrategy (str:string) = 
      match str.ToLower() with 
      | "standard" -> DeletionStrategy.RwsSetbased, false
      | "accuracy-based" -> DeletionStrategy.RwsFitness, true
      | "random-with-accuracy" -> DeletionStrategy.RandomWithAccuracy, true
      | "random" -> DeletionStrategy.Random, false
      | _ -> DeletionStrategy.Random, false
   
   
   type ExperimentParameters = 
      {
         mutable totalSteps : int
         mutable totalLearningSteps : int
         totalTime : int
         mutable problemSteps : int
         mutable totalReward : double
         mutable systemError : double
         maxAction : int
      }
   with
      static member Empty =
         {
               totalSteps = 0
               totalLearningSteps =  0
               totalTime =  0
               problemSteps =  0
               totalReward =  0.0
               systemError =  0.0
               maxAction =  8
         }
      static member FromParameters (pms:Parameters) =
         {
            totalSteps = pms.TryGetInteger "total experiment steps" 0
            totalLearningSteps = pms.TryGetInteger "total learning steps" 0
            totalTime = pms.TryGetInteger "total time" 0
            problemSteps = pms.TryGetInteger "problem steps"  0
            totalReward = pms.TryGetDouble "total reward" 0.0
            systemError = pms.TryGetDouble "system error" 0.0
            maxAction = pms.TryGetInteger "max action" 8
         } 

   type PopulationParameters = 
      {
          maxPopulation  : int
          mutable populationSize:int
          mutable macroSize:int
          populationInit : PopInitStrategy
          populationInitFile: string
      }
   with 
      static member Empty =
         {
            maxPopulation = 100
            populationSize =  0
            macroSize =  0
            populationInit = PopInitStrategy.Empty
            populationInitFile = "classifiers.txt"
         }
      static member FromParameters (pms:Parameters) = 
         {
            maxPopulation = pms.TryGetInteger "max population size" 100
            populationSize =  0
            macroSize =  0
            populationInit =   SelectPopInitStrategy (pms.TryGetString "population init" "default")
            populationInitFile = pms.TryGetString "population init file" "classifiers.txt"
         }

   type ClassifierParameters = 
      {
         initPrediction : double
         initError : double
         initFitness : double
         initSetSize : double
         initNumUpdates : int
         classifierWidth : int
         //fitness computation parameters
         epsilonZero : double
         alpha : double
         vi:double
         useExponentialFitness :bool
         // reinforcement parameters
         learningRate :double
         discountFactor :double
      }
   with 
      static member Empty = 
         {
            initPrediction = 0.0
            initError =  0.0
            initFitness =  0.0
            initSetSize =  0.0
            initNumUpdates =  0
            classifierWidth =  24
            //fitness computation parameters
            epsilonZero = 0.0
            alpha = 0.0
            vi = 0.0
            useExponentialFitness = false
            //reinforcement parameters
            learningRate = 0.8
            discountFactor = 0.2
         }
      static member FromParameters (pms:Parameters) = 
         {
            initPrediction = pms.TryGetDouble "prediction init" 0.0
            initError = pms.TryGetDouble "error init" 0.0
            initFitness = pms.TryGetDouble "fitness init" 0.0
            initSetSize = pms.TryGetDouble "set size init" 0.0
            initNumUpdates = pms.TryGetInteger "number updates init" 0
            classifierWidth = pms.TryGetInteger "classifier width" 24
            //fitness computation parameters
            epsilonZero = pms.TryGetDouble "epsilon zero"  0.0
            alpha = pms.TryGetDouble "alpha" 0.0
            vi = pms.TryGetDouble "vi" 0.0
            useExponentialFitness = pms.TryGetBool "use exponential fitness" false
            // reinforcement parameters
            learningRate = pms.TryGetDouble "learning rate" 0.8
            discountFactor = pms.TryGetDouble "discount factor" 0.2
         }


   type StrategyParameters = 
      {
         coveringStrategy : CoveringStrategy
         tethaNma : double
         fractionForCovering :double
         actionSelectionStrategy : ActionSelectionStrategy
         prbRnd : double
         explorationStrategy : ExplorationStrategy
         probRandomAction : double 
      }
   with
      static member Empty = 
         {
            coveringStrategy = CoveringStrategy.Standard
            tethaNma =  0.0
            fractionForCovering =  0.0
            actionSelectionStrategy = ActionSelectionStrategy.Uniform
            prbRnd = 0.0
            explorationStrategy = ExplorationStrategy.Explore
            probRandomAction = 0.0
         }

      static member FromParameters (pms:Parameters) = 
         let strategy, prob = SelectActionStrategy (pms.TryGetString " " "proportional")
         {
            coveringStrategy = SelectCoverStrategy (pms.TryGetString "covering strategy" "standard")
            tethaNma = pms.TryGetDouble "covering threshold" 0.0
            fractionForCovering = pms.TryGetDouble "fraction for covering" 0.0
            actionSelectionStrategy = strategy
            prbRnd = match prob with Some v -> v | None -> 0.0
            explorationStrategy = SelectExplorationStrategy (pms.TryGetString "exploration strategy" "greedy")
            probRandomAction = pms.TryGetDouble "prob random action" (match prob with | Some f -> f | None -> 0.0)
         }

   type GaParameters = 
      {
         discoveryComponent : DiscoveryStrategy
         flagDiscoveryComponent : bool
         thetaGA : double
         propCrossover : double
         probMutation : double 
         flagGaAvgInit : bool 
         flagErrorUpdateFirst : bool
         flagUpdateTest : bool
         useGa : bool
         useCrossover : bool 
         useMutation : bool
      }
   with
      static member Empty = 
         {
            discoveryComponent = DiscoveryStrategy.NoDiscovery
            flagDiscoveryComponent =  true
            thetaGA =  0.0
            propCrossover =  0.0
            probMutation =  0.0
            flagGaAvgInit =  false
            flagErrorUpdateFirst =  false
            flagUpdateTest =  false
            useGa =  true
            useCrossover =  true
            useMutation =  true
         }
      static member FromParameters (pms:Parameters) = 
         {
            discoveryComponent = SelectDiscoveyComponent(pms.TryGetString "discovery component" "default")
            flagDiscoveryComponent = pms.TryGetBool " " true;
            thetaGA = pms.TryGetDouble "theta GA" 0.0
            propCrossover = pms.TryGetDouble "crossover probability" 0.0
            probMutation = pms.TryGetDouble "mutation probability" 0.0
            flagGaAvgInit = pms.TryGetBool " " false
            flagErrorUpdateFirst = pms.TryGetBool "update error first" false
            flagUpdateTest = pms.TryGetBool "update during test" false
            useGa = pms.TryGetBool "use GA" true
            useCrossover = pms.TryGetBool "use crossover" true
            useMutation = pms.TryGetBool "use mutation" true
         }

   type DeletionParameters = 
      {
         //subsumption deletion parameters
         flagGaSubsumption :bool
         flagGaaSubsumption :bool
         flagAsSubsumption :bool
         thetaGaSub : double
         thetaAsSub: double
         flagCoverAvgInit :bool
         //delete parameters
         deletionStrategy : DeletionStrategy
         // allow parameters to override default setting
         flagDeleteWithAccuracy :bool
         thetaDel : double
         deltaDel : double
      }
   with
      static member Empty = 
         {
            //subsumption deletion parameters
            flagGaSubsumption =  true
            flagGaaSubsumption = false
            flagAsSubsumption =  true
            thetaGaSub =  0.0
            thetaAsSub =  0.0
            flagCoverAvgInit = true

            //delete parameters
            deletionStrategy = DeletionStrategy.RwsSetbased
            // allow parameters to override default setting
            flagDeleteWithAccuracy = false
            thetaDel =  0.0
            deltaDel =  0.0
         }

      static member FromParameters (pms:Parameters) =
         let strategy, flag = SelectDeletionStrategy (pms.TryGetString "deletion strategy" "default")
         {
            //subsumption deletion parameters
            flagGaSubsumption = pms.TryGetBool "GA Subsumption" true
            flagGaaSubsumption = pms.TryGetBool "GAA Subsumption" false
            flagAsSubsumption = pms.TryGetBool "AS Subsimption" true
            thetaGaSub = pms.TryGetDouble "theta GA sub" 0.0
            thetaAsSub = pms.TryGetDouble "theta As sub" 0.0
            flagCoverAvgInit = pms.TryGetBool " " true

            //delete parameters
            deletionStrategy = strategy
            // allow parameters to override default setting
            flagDeleteWithAccuracy = pms.TryGetBool "delete with accuracy" false
            thetaDel = pms.TryGetDouble "theta delete" 0.0
            deltaDel = pms.TryGetDouble "delta delete" 0.0
         }


   type StaticClassData(clss:string, tag:string) =
      let mutable  classData =  ClassData.NewClassData clss tag null
      let mutable initialized = false
      let mutable nextId = 0
      member x.Initialized with get() =  initialized and set(v) = initialized <- v
      member x.TagName = classData.TagName
      member x.ClassName = classData.ClassName
      member x.SetParameters pms = 
         do classData <- ClassData.NewClassData clss tag pms
         do initialized <- true
         x

      member x.Parameters = classData.parameters
      member x.NextId() = 
         lock x (fun () -> 
            let ret = nextId
            do nextId <- nextId + 1
            ret)
   