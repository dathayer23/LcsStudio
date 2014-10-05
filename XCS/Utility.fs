namespace BamaLlama.XCS

open System
[<AutoOpen>]
module Utility = 
   type either<'a,'b> = Right of 'a | Left of 'b
   let rnd = new Random()
   let dice n = rnd.Next(1,n+1)
   let coinToss() = rnd.Next() % 2 = 0

   let intFromStr str = match Int32.TryParse(str) with (true,v) -> Some v | (false,_) -> None
