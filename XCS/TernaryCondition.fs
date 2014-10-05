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
   
  
   
   type BinaryPattern(pat : patternValue []) = 
      let pattern = pat
      let size = (Array.length pat)

      //binaryPattern is a Pattern
      interface IPattern<patternValue> with 
         member x.Size = size
         member x.Pattern = pattern
      
      //that can be randomized
      interface IRandomizable<BinaryPattern> with
         member x.Random() = new BinaryPattern(Array.init ((int)(x :> IPattern<_>).Size) (fun _ -> patternValue.Random()))
         

   type TrinaryPattern(pat : classifierValue []) = 
      let pattern = pat
      let size = (Array.length pat)
      //Trinary Pattern is a Pattern
      interface IPattern<classifierValue> with
         member x.Size = size
         member x.Pattern = pattern

      member x.Size = (x :> IPattern<classifierValue>).Size
      member x.Pattern = (x :> IPattern<classifierValue>).Pattern
      //that can be randomized 
      interface IRandomizable<TrinaryPattern> with
         member x.Random() = new TrinaryPattern(Array.init ((int)(x :> IPattern<_>).Size) (fun _ -> classifierValue.Random()))
      member x.Random() = (x :> IRandomizable<TrinaryPattern>).Random()
         
   let GetMutationType i = 
      match i with 
      | 1 -> mutationType.Uniform
      | 2 -> mutationType.TwoValue
      | 3 -> mutationType.ThreeValue
      | _ -> failwith "Invalid Mutation Type"
   
//   let GetCrossoverType i = 
//      match i with 
//      | 0 -> 
   type TernaryCondition(pat, parms:Parameters) =
      static let classData = new ClassData("TernaryCondition", "condition:Ternary")
      
      let mutable dontCareProb : double = parms.TryGetDouble "DontCareProb" 0.25
      let mutable crossoverType : int = parms.TryGetInteger "CrossoverType" 1
      let mutable mutationType : mutationType = GetMutationType(parms.TryGetInteger "MutationType" 1)
      let mutable flagMutationWithDontCare = parms.TryGetBool "FlagMutationWithDontCare" true
      let mutable pattern = pat
      
      new (size: int) = TernaryCondition(TrinaryPattern(Array.init size (fun _ -> classifierValue.Random true)), Parameters())
      //new (pat: TrinaryPattern, parms) = new TernaryCondition((pat :> IPattern<_>).Size, pat, parms)
      
      interface IPattern<classifierValue> with
         member x.Pattern = (pattern :> IPattern<_>).Pattern
         member x.Size = Array.length (pattern :> IPattern<_>).Pattern
      
      member x.Size = (x :> IPattern<classifierValue>).Size
      member x.Pattern = (x :> IPattern<classifierValue>).Pattern
      member x.Specificness = 
        Array.sumBy 
         (fun (x:classifierValue) -> 
            if x.isDontCare 
            then 0 else 1) x.Pattern

      member x.Match (y:IPattern<patternValue>) = 
         do assert((x :> IPattern<_>).Size = y.Size)
         Array.forall2 (fun (xc:classifierValue) (yc:patternValue) -> matchValue xc yc) x.Pattern y.Pattern 

      member x.Matches (y:TernaryCondition) = 
         do assert( ((x :> IPattern<_>).Size) = ((y :> IPattern<_>).Size) )
         Array.forall2 (fun (xv:classifierValue) yv -> xv.Match yv) x.Pattern (y :> IPattern<_>).Pattern

      member x.Cover (y: IPattern<_>) = 
         do assert(y.Size = x.Size)      
         
         let pat = new TrinaryPattern(Array.init ((int)y.Size) 
            (fun i ->  
               if Utility.rnd.NextDouble() < dontCareProb 
               then DontCare 
               else classifierValueOf (y.Pattern.[i]) ))

         (new TernaryCondition(pat, parms)) 
         

      member x.Mutate mu = 
         let pat = 
            match mutationType with 
            | mutationType.ThreeValue -> 
               Array.map 
                  (fun x ->  
                     if Utility.rnd.NextDouble() < mu 
                     then  classifierValue.Random flagMutationWithDontCare 
                     else x) (x :> IPattern<_>).Pattern         
                       
            | mutationType.TwoValue -> 
               Array.map 
                  (fun (x:classifierValue) ->  
                     if Utility.rnd.NextDouble() < mu 
                     then x.RandomOther flagMutationWithDontCare 
                     else x) (x :> IPattern<_>).Pattern

         (new TernaryCondition(TrinaryPattern(pat), parms))    

      member x.Mutate ((y:BasePattern, mu)) = 
         assert (y.Length = x.Size)
         match mutationType with 
         | mutationType.Uniform -> 
            let pat = 
               Array.map2 
                  (fun xv yv -> 
                     if Utility.rnd.NextDouble() < mu 
                     then mutate1 xv yv flagMutationWithDontCare 
                     else xv) x.Pattern y.Pattern
            (new TernaryCondition(TrinaryPattern(pat), parms))

         | _ -> x.Mutate mu

      interface IRandomizable<TernaryCondition> with
         member x.Random() = 
            let pat = 
               Array.init (x :> IPattern<classifierValue>).Size 
                  (fun _ -> 
                     if Utility.rnd.NextDouble() < dontCareProb 
                     then DontCare 
                     else classifierValue.Random false)

            (new TernaryCondition(TrinaryPattern(pat), parms))

      member this.Recombine (y:TernaryCondition) recombType =
         assert (this.Size = y.Size)
         let uniformCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            Array.map2 
               (fun xv yv -> 
                  if Utility.rnd.NextDouble() < 0.5 
                  then xv else yv) x.Pattern y.Pattern

         let singlePointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt = Utility.dice x.Size
            Array.mapi2 (fun i xv yv -> if i < pt then xv else yv) x.Pattern y.Pattern
         
         let twoPointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt1,pt2 = Utility.dice x.Size, Utility.dice x.Size
            let p1,p2 = if pt1 = pt2 then pt1, min (pt2 + 2 ) x.Size else pt1,pt2
            let pp1,pp2 = if p1 > p2 then p2,p1 else p1, p2
            Array.mapi2 (fun i xv yv -> if i > pp1 && i < pp2 then yv else xv) x.Pattern y.Pattern
            
         //match y with 
         //| :? TernaryCondition as cond -> 
         let pat = 
            match recombType with 
            | Uniform ->  uniformCrossover this y
            | OnePoint -> singlePointCrossover this y
            | TwoPoint -> twoPointCrossover this y

         (new TernaryCondition(TrinaryPattern(pat), parms)) //:> CondBase 
         //| _ -> raise (TernaryConditionException "Invalid pattern type in Match function")

      member x.SetStringValue (str:string) = 
         do pattern <- TrinaryPattern(Array.map (fun ch -> valueOfChar ch) (str.ToCharArray()))
         ()

      member x.StringValue() = Array.map (fun v -> charOfValue v) x.Pattern |> fun (chs:char []) -> new String(chs)

      member x.SubsumedBy (y:TernaryCondition) = 
         assert (x.Size = y.Size)
         //match y with 
         //| :? TernaryCondition as cond -> 
         x.Matches y && y.IsMoreGeneralThan x
         //| _ -> raise (TernaryConditionException "Invalid pattern type in SubsumedBy function")

      member x.IsMoreGeneralThan (y:TernaryCondition) = 
         assert (x.Size = y.Size)
         Array.forall2 (fun xv yv -> xv = DontCare || xv = yv) x.Pattern y.Pattern
         //match y with 
         //| :? TernaryCondition as cond -> Array.forall2 (fun xv yv -> xv = DontCare || xv = yv) x.Pattern cond.Pattern
            