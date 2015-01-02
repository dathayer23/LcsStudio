namespace BamaLlama.XCS

open System
[<AutoOpen>]
module Utility = 
   type either<'a,'b> = Right of 'a | Left of 'b
   let rnd = new Random()
   let dice n = rnd.Next(1,n+1)
   let coinToss() = rnd.Next() % 2 = 0

   let intFromStr str = match Int32.TryParse(str) with (true,v) -> Some v | (false,_) -> None
   let dblFromStr str = match System.Double.TryParse(str) with (true,v) -> Some v | (false,_) -> None
   let dblFromStrWithDefault str v = match dblFromStr str with Some x -> x | None -> v
   let intFromStrWithDefault str v = match intFromStr str with Some x -> x | None -> v
   let biasedCoin dbl = rnd.NextDouble() < dbl 
   let long2binary decimal size = 
      let _base = 1 <<< size
      let rec _long2binary str bit bse = 
          if bit < size 
          then  
            let b = bse >>> 1
            _long2binary (str + (if (decimal &&& b) > 0 then "1" else "0")) (bit + 1) b
          else 
             str

      _long2binary "" 0 _base



              