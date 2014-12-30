namespace BamaLlama.XCS

open System
open Base
open Params 
module Action = 
   exception ActionException of string

   
   type ActionBase() = 
      let mutable value : int = 0
      member x.RandomAction opint = new ActionBase()
      member x.Value with get() = value and set(v) = value <- v
      member x.Next () = 0

      //abstract member ActionFromString : string -> ActionBase

   type Action(i, parms:Parameters) = 
      //inherit ActionBase()
      static let ClassData = new ClassData("binary_action","action::binary")
      let maxAction = parms.TryGetInteger "MaxAction" 8 
      let mutable action:int =  i % maxAction
      
      new(i) =  Action(i, new Parameters())
      member x.Value with get() = action and set(v) = action <- v
      member x.MaxAction = maxAction

      member x.Next : unit -> int = fun () ->
         do action <- (action + 1) % maxAction
         action

      member x.Random() = new Action( Utility.dice(maxAction - 1))          

      static member ActionFromString (str:string)  = 
         let ints = str.Split([|':'|]) |> Array.map Utility.intFromStr
         match ints with 
         | [|Some v1; Some v2|] ->
            let parms = new Parameters()
            do parms.Add "MaxAction" (Int v2)
            (new Action(v1, parms))

         | [|Some v1; None|] -> (new Action(v1))
         | [|None; Some v2|] -> (new Action(v2))
         | [|None; None|] -> raise (ActionException (sprintf "Invalid string input in ActionFromString '%s'" str))

         
      override x.ToString() = sprintf "%d:%d" action maxAction