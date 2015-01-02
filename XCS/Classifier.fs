namespace BamaLlama.XCS

open System
open System.IO
open Microsoft.FSharp.Core.Operators
open Base
open Params
open Interfaces
open Action
open ConditionBase
open TernaryCondition

module Classifier = 
   
   type StaticClassifierData(clss:string, tag:string) =
      static let mutable  classData =  ClassData.NewClassData "" ""
      let mutable nextId = 0
      member x.TagName = classData.TagName
      member x.ClassName = classData.ClassName
      member x.SetParameters pms = classData.SetParameters pms
      member x.Parameters = classData.parameters
      member x.NextId() = 
         lock x (fun () -> 
            let ret = nextId
            do nextId <- nextId + 1
            ret)
   

   type classifier(cond : TernaryCondition , act:Action) = 
      static let classData = new StaticClassifierData("xcs_classifier","classifier")
      static let SetParameters (pms: ParameterDB) =  classData.SetParameters pms

      static let parms = classData.Parameters
      //static let mutable recombinationType = recombinationType.Uniform 
      static let Init( prms : ParameterDB) = 
         do classData.SetParameters prms
         //do recombinationType <- parms.TryGetInteger "recombination type" 1
         ()

      let identifier = classData.NextId()

      let mutable (condition : TernaryCondition) = cond
      let mutable (action : Action) = act
      let mutable prediction = 0.0
      let mutable error = 0.0
      let mutable fitness = 0.0
      let mutable actionSetSize = 0.0
      let mutable experience = 0L
      let mutable numerosity = 1L
      let mutable timeStamp = 0L
      
      /// Constructors
      new (size:int, maxAction) = new classifier(new TernaryCondition(size), new Action(dice maxAction))

      /// properties
      member x.Id = identifier
      member x.ClassName : string = classData.ClassName
      member x.TagName = classData.TagName
      member x.Prediction with get() = prediction and set v = prediction <- v
      member x.Error with get() = error and set v = error <- v
      member x.Fitness with get() = fitness and set v = fitness <- v
      member x.ActionSetSize with get() = actionSetSize and set v = actionSetSize <- v
      member x.Experience with get() = experience and set v = experience <- v
      member x.Numerosity with get() = numerosity and set v = numerosity <- v
      member x.TimeStamp with get() = timeStamp and set v = timeStamp <- v
      member x.TotalPrediction  = x.Prediction * (double)x.Numerosity
      member x.Condition with get() = condition and set(v) = condition <- v
      member x.Action with get() =  action  and set(v) = action <- v

      static member ReadState (sr:StreamReader) = null
      member x.WriteState (sw:StreamWriter) = ()

      member x.Random() =  new classifier(condition.Random(), Action.Random())
      member x.Match (pattern:IPattern<_>) = condition.Match(pattern)
      
      static member Cover (pattern:BinaryPattern) : classifier = 
         new classifier(TernaryCondition.Cover(pattern, 0.2), Action.Random())
      

      member x.Mutate mutationProb inputs = 
         do condition <- condition.Mutate ((inputs, mutationProb))
         do action.Mutate(mutationProb)
         ()

      member x.Recombine( classifier:classifier ) = 
         do condition <- condition.Recombine(classifier.Condition)
         let tmp = classifier.Action
         do classifier.Action <- action
         do action <- tmp

      member x.Subsume (classifier:classifier) = 
         action = classifier.Action && 
         condition.IsMoreGeneralThan(classifier.Condition)
      
      override x.ToString() = 
         sprintf "%d,%s|%s,%f,%f,%f,%f,%d,%d"
            x.Id (x.Condition.ToString()) (x.Action.ToString()) 
            prediction error fitness actionSetSize experience numerosity

      member x.Print (sw:StreamWriter) = sw.WriteLine(x.ToString())

      static member FromString( str:string ) = 
         let fields = str.Split([|','|])
         let id = Utility.intFromStrWithDefault fields.[0] -1
         let cond_act = fields.[1].Split([|'|'|])
         let cond = new TernaryCondition(cond_act.[0])
         let action = Action.Action.ActionFromString(cond_act.[1])

         ()
