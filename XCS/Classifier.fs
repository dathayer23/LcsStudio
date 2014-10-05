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
      member x.ClassName : string = classData.ClassName
      member x.TagName = classData.TagName
      
      member x.Random() =  new classifier(condition.RandomCondition None, action.RandomAction None)
      
      
      