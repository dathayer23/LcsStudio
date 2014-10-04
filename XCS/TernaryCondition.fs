namespace BamaLlama.XCS

open System
open Utility
open Interfaces
open Base
open Params
open Values
open ConditionBase 

module TernaryCondition =
   exception TernaryConditionException of string
   
  
   
   type BinaryPattern(size:uint32, pat : patternValue []) = 
      let pattern = pat

      //binaryPattern is a Pattern
      interface IPattern<patternValue> with 
         member x.Size = size
         member x.Pattern = pattern
      
      //that can be randomized
      interface IRandomizable<BinaryPattern> with
         member x.Random() = new BinaryPattern((x :> IPattern<_>).Size, Array.init ((int)(x :> IPattern<_>).Size) (fun _ -> patternValue.Random()))
         

   type TrinaryPattern(size:uint32, pat : classifierValue []) = 
      let pattern = pat

      //Trinary Pattern is a Pattern
      interface IPattern<classifierValue> with
         member x.Size = size
         member x.Pattern = pattern

      //that can be randomized 
      interface IRandomizable<TrinaryPattern> with
         member x.Random() = new TrinaryPattern((x :> IPattern<_>).Size, Array.init ((int)(x :> IPattern<_>).Size) (fun _ -> classifierValue.Random()))
         

   
   type TernaryCondition(width:uint32, pat, parms:Parameters) =
      //inherit CondBase(width)
      static let classData = new ClassData("TernaryCondition", "condition:Ternary")
      
      let mutable dontCareProb : double = parms.TryGetDouble "DontCareProb" 0.25
      let mutable crossoverType : int = parms.TryGetInteger "CrossoverType" 1
      let mutable mutationType : int = parms.TryGetInteger "MutationType" 1
      let mutable flagMutationWithDontCare = parms.TryGetBool "FlagMutationWithDontCare" true
      let mutable pattern = pat
      
      new (pat: TrinaryPattern, parms) = new TernaryCondition((pat :> IPattern<_>).Size, pat, parms)
      //new (size) = new TernaryCondition(size, (Array.init size (fun _ -> classifierValue.Random true), [])

      interface IPattern<classifierValue> with
         member x.Pattern = (pattern :> IPattern<_>).Pattern
         member x.Size = width
      
      member x.Specificness = Array.sumBy (fun (x:classifierValue) -> if x.isDontCare then 0 else 1) (x :> IPattern<_>).Pattern

      member x.Match (y:IPattern<patternValue>) = 
         do assert((x :> IPattern<_>).Size = y.Size)
         Array.forall2 (fun (xc:classifierValue) (yc:patternValue) -> matchValue xc yc) (x :> IPattern<_>).Pattern y.Pattern 

      member x.Matches (y:TernaryCondition) = 
         do assert( ((x :> IPattern<_>).Size) = ((y :> IPattern<_>).Size) )
         Array.forall2 (fun (xv:classifierValue) yv -> xv.Match yv) (x :> IPattern<_>).Pattern (y :> IPattern<_>).Pattern

      member x.Cover (y: IPattern<_>) = 
         do assert(y.Size = (x :> IPattern<_>).Size)      
         
         let pat = new TrinaryPattern(y.Size, Array.init ((int)y.Size) (fun i ->  if Utility.rnd.NextDouble() < dontCareProb then DontCare else classifierValueOf (y.Pattern.[i]) ))
         (new TernaryCondition(pat, parms)) 
         

      member x.Mutate mu = 
         let pat = 
            match mutationType with 
            | 3 -> Array.map (fun x ->  if Utility.rnd.NextDouble() < mu then  classifierValue.Random flagMutationWithDontCare else x) x.Pattern            
            | 2 -> Array.map (fun (x:classifierValue) ->  if Utility.rnd.NextDouble() < mu then x.RandomOther flagMutationWithDontCare else x) x.Pattern
         (new TernaryCondition(pat, parms))    

      member x.Mutate ((y:BasePattern, mu)) = 
         assert (y.Length = x.Size)
         match mutationType with 
         | 1 -> 
            let pat = Array.map2 (fun xv yv -> if Utility.rnd.NextDouble() < mu then mutate1 xv yv flagMutationWithDontCare else xv) x.Pattern y.Pattern
            (new TernaryCondition(pat, parms))
         | _ -> x.Mutate mu

      interface IRandomizable<TernaryCondition> with
         member x.Random() = 
            let pat = Array.init x.Size (fun _ -> if Utility.rnd.NextDouble() < dontCareProb then DontCare else classifierValue.Random false)
            (new TernaryCondition(pat, parms))

      member this.Recombine (y:CondBase) recombType =
         assert (this.Size = y.Size)
         let uniformCrossover (x:TernaryCondition) (y:TernaryCondition) = Array.map2 (fun xv yv -> if Utility.rnd.NextDouble() < 0.5 then xv else yv) x.Pattern y.Pattern
         let singlePointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt = Utility.dice x.Size in  Array.mapi2 (fun i xv yv -> if i < pt then xv else yv) x.Pattern y.Pattern
         
         let twoPointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt1,pt2 = Utility.dice x.Size, Utility.dice x.Size
            let p1,p2 = if pt1 = pt2 then pt1, min (pt2 + 2 ) x.Size else pt1,pt2
            let pp1,pp2 = if p1 > p2 then p2,p1 else p1, p2
            Array.mapi2 (fun i xv yv -> if i > pp1 && i < pp2 then yv else xv) x.Pattern y.Pattern
            
         match y with 
         | :? TernaryCondition as cond -> 
            let pat = 
               match recombType with 
               | Uniform ->  uniformCrossover this cond
               | OnePoint -> singlePointCrossover this cond
               | TwoPoint -> twoPointCrossover this cond

            (new TernaryCondition(pat, parms)) :> CondBase 
         | _ -> raise (TernaryConditionException "Invalid pattern type in Match function")

      member x.SetStringValue (str:string) = 
         do pattern <- Array.map (fun ch -> valueOfChar ch) (str.ToCharArray())
         ()

      member x.StringValue() = Array.map (fun v -> charOfValue v) x.Pattern |> fun (chs:char []) -> new String(chs)

      member x.SubsumedBy (y:CondBase) = 
         assert (x.Size = y.Size)
         match y with 
         | :? TernaryCondition as cond -> 
               x.Matches cond && cond.IsMoreGeneralThan x
         | _ -> raise (TernaryConditionException "Invalid pattern type in SubsumedBy function")

      member x.IsMoreGeneralThan (y:CondBase) = 
         assert (x.Size = y.Size)
         match y with 
         | :? TernaryCondition as cond -> Array.forall2 (fun xv yv -> xv = DontCare || xv = yv) x.Pattern cond.Pattern
            