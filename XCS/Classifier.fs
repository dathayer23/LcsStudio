namespace BamaLlama.XCS

open Microsoft.FSharp.Core.Operators
open Base
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
   
   let inline randomCond opint :  ^a
         = (^a : (static member RandomCond : int option -> ^a) opint) 

   let inline randomAction opint 
         = (^a : (static member RandomAction : int option -> ^a) opint) 

   type classifier<'TCond, 'TAction when 'TCond :> CondBase and 'TAction :> ActionBase>(cond , act) = 
      static let classData = new StaticClassifierData("xcs_classifier","classifier")

      let condition : 'TCond = cond
      let action : 'TAction = act
      member x.Dummy = ()
      member x.ClassName : string = classData.ClassName
      member x.TagName = classData.TagName
      member x.Random() = 
         let new_cond : 'TCond = (condition : #CondBase).RandomCondition None
         let new_action : 'TAction = action.RandomAction None
         new classifier<_,_>(new_cond, new_action)

      
      