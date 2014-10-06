namespace BamaLlama.XCS

open Microsoft.FSharp.Core.Operators
open Base
open Interfaces
open Action
open ConditionBase

module Classifier = 
   
   type StaticClassifierData(clss:string, tag:string) =
      inherit ClassData(clss, tag)
      let mutable nextId = 0
      
      member x.NextId() = 
         lock x (fun () -> 
            let ret = nextId
            do nextId <- nextId + 1
            ret)
   
//   let inline randomCond opint :  ^a
//         = (^a : (static member RandomCond : int option -> ^a) opint) 
//
//   let inline randomAction opint 
//         = (^a : (static member RandomAction : int option -> ^a) opint) 

   type classifier//<'Action, 'TCond, 'TAction 
      //when 'TCond :> ICondition 
      //and 'TAction :> IAction<'Action>>
      (cond , act) = 

      static let classData = new StaticClassifierData("xcs_classifier","classifier")

      let (condition : ICondition) = cond
      let (action : IAction<int>) = act
      let identifier = classData.NextId()

      let mutable prediction = 0.0
      let mutable error = 0.0
      let mutable fitness = 0.0
      let mutable actionSetSize = 0.0
      let mutable experience = 0L
      let mutable numerosity = 0L
      let mutable timeStamp = 0L

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

      member x.Random() =  new classifier(condition.RandomCondition None, action.RandomAction None)
      
      
      