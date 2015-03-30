namespace BamaLlama.XCS

open System
open System.Text
open Utility
open Interfaces
open Base
open Params
open Values
open ConditionBase 

module TernaryCondition =
   exception TernaryConditionException of string
   
   //----------------------------------------------------------------------------------------------------------------
   /// Sequences of 0's and 1's
   type BinaryPattern(pat : patternValue []) = 
      let pattern = pat
      let size = (Array.length pat)

      new(size) = new BinaryPattern(Array.init size (fun _ -> patternValue.Random()))
      new (str:string) = 
         new BinaryPattern(
            str.ToCharArray() 
            |> Array.map (fun c -> patternValue.OfChar c))


      //binaryPattern is a Pattern
      interface IPattern<patternValue> with 
         member x.Size = size
         member x.Pattern = pattern
      member x.Item with get(index) = pattern.[index]

      member x.Size = (x :> IPattern<patternValue>).Size
      member x.Pattern = (x :> IPattern<patternValue>).Pattern

      //that can be randomized
      interface IRandomizable<BinaryPattern> with
         member x.Random() = new BinaryPattern(Array.init ((int)(x :> IPattern<_>).Size) (fun _ -> patternValue.Random()))
      override x.ToString() = 
         Array.fold (fun (a:StringBuilder) s -> a.Append(s.ToString())) (new StringBuilder(size)) pattern
         |> fun sb -> sb.ToString()
   
   //-----------------------------------------------------------------------------------------------------------------------
   /// Sequences of 0's and 1's and don't care values
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
         member __.Random() = new TrinaryPattern(Array.init size (fun _ -> classifierValue.Random()))
      member x.Random() = (x :> IRandomizable<TrinaryPattern>).Random()
      override __.ToString() = 
         Array.fold (fun (a:StringBuilder) s -> a.Append(s.ToString())) (new StringBuilder(size)) pattern
         |> fun sb -> sb.ToString()
        
      static member FromString(str:string) = TrinaryPattern(str.ToCharArray() |> Array.map (fun c -> classifierValue.OfChar c))    
      
      static member Cover (pat:BinaryPattern) prob =         
            new TrinaryPattern( 
                  Array.map (fun tok -> 
                           if biasedCoin prob 
                           then classifierValueOf tok 
                           else classifierValue.DontCare) pat.Pattern)  


   let GetMutationType i = 
      match i with 
      | 1 -> mutationType.Uniform
      | 2 -> mutationType.TwoValue
      | 3 -> mutationType.ThreeValue
      | _ -> failwith "Invalid Mutation Type"
   
   let GetRecombinationType i = 
      match i with 
      | 1 -> recombinationType.Uniform
      | 2 -> recombinationType.OnePoint
      | 3 -> recombinationType.TwoPoint
      | _ -> failwith "Invalid recombination type"

      
   //----------------------------------------------------------------------------------------------------------------------------
   /// ternary pattern with an associated action and a specific set of classData.parameters
   type TernaryCondition(pat:TrinaryPattern) =
      static let mutable classData = new StaticClassData("TernaryCondition","condition::Ternary")
      //inherit ParameterizedClass()

      static let  mutable dontCareProb : double =  0.25
      static let mutable crossoverType : recombinationType  = recombinationType.Uniform
      static let mutable mutationType : mutationType = mutationType.Uniform
      static let mutable flagMutationWithDontCare =  true
      let mutable pattern = pat
      
      new (size: int) = TernaryCondition(TrinaryPattern(Array.init size (fun _ -> classifierValue.Random true)))
      new (pattern:string) = TernaryCondition(TrinaryPattern.FromString(pattern))

      static member NewTernaryCondition(``params``, (pat:TrinaryPattern)) = 
         let tcnd = new TernaryCondition(pat)
         if TernaryCondition.Initialized then tcnd
         else
            do TernaryCondition.Init(``params``)
            tcnd

      static member NewTernaryCondition(``params``, (size:int)) = 
         let tcnd = new TernaryCondition(size)
         if TernaryCondition.Initialized then tcnd
         else
            do TernaryCondition.Init(``params``)
            tcnd

      static member private Init(parameterDB:ParameterDB) =
         do classData <- classData.SetParameters parameterDB
         do dontCareProb <- classData.Parameters.TryGetDouble "DontCareProb" 0.25
         do crossoverType <- GetRecombinationType (classData.Parameters.TryGetInteger "CrossoverType" 1)
         do mutationType <- GetMutationType(classData.Parameters.TryGetInteger "MutationType" 1)
         do flagMutationWithDontCare <- classData.Parameters.TryGetBool "FlagMutationWithDontCare" true
         do classData.Initialized <- true
         
      static member Initialized = classData.Initialized
      member x.Size = pattern.Size
      member x.Pattern = pattern.Pattern
      member x.Specificness = 
        Array.sumBy 
         (fun (x:classifierValue) -> 
            if x.isDontCare 
            then 0 else 1) x.Pattern
      
      member x.Genericness = 1.0 - (float)x.Specificness/ (float)x.Size
      member x.Match (y:IPattern<patternValue>) : bool = 
         do assert(x.Size = y.Size)
         Array.forall2 (fun (xc:classifierValue) (yc:patternValue) -> matchValue xc yc) x.Pattern y.Pattern 

      member x.Matches (y:TernaryCondition) = 
         do assert(x.Size = y.Size)
         Array.forall2 (fun (xv:classifierValue) yv -> xv.Match yv) x.Pattern y.Pattern

      member x.Cover (y: IPattern<_>) = 
         do assert(y.Size = x.Size)      
         
         let pat = 
          new TrinaryPattern(Array.init ((int)y.Size) 
            (fun i ->  
               if Utility.rnd.NextDouble() < dontCareProb 
               then DontCare 
               else classifierValueOf (y.Pattern.[i]) ))

         (new TernaryCondition(pat)) 

      member x.Mutate (mu, (?y:BasePattern)) : TernaryCondition = 
         let mutate3() = 
            Array.map 
               (fun x ->  
                  if biasedCoin mu 
                  then  classifierValue.Random flagMutationWithDontCare 
                  else x) x.Pattern   

         let mutate2() = 
            Array.map 
               (fun (x:classifierValue) ->  
                  if biasedCoin mu 
                  then x.RandomOther flagMutationWithDontCare 
                  else x) x.Pattern

         let mutateU (y:BasePattern) =             
             Array.map2 
               (fun xv yv -> 
                  if biasedCoin mu 
                  then mutate1 xv yv flagMutationWithDontCare 
                  else xv) x.Pattern y.Pattern
            
         let pat = 
            match mutationType with 
            | mutationType.Uniform when Option.isSome(y) -> 
               assert (y.Value.Length = x.Size)
               mutateU y.Value

            | mutationType.ThreeValue -> mutate3()
            | mutationType.TwoValue -> mutate2()    
                  
         (new TernaryCondition(TrinaryPattern(pat)))

      interface IRandomizable<TernaryCondition> with
         member x.Random() = 
            let pat = 
               Array.init x.Size 
                  (fun _ -> 
                     if biasedCoin dontCareProb 
                     then DontCare 
                     else classifierValue.Random false)

            (new TernaryCondition(TrinaryPattern(pat)))
      member x.Random() =  (x :> IRandomizable<TernaryCondition>).Random()

      member this.Recombine (y:TernaryCondition) =
         assert (this.Size = y.Size)
         let uniformCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            Array.map2 
               (fun xv yv -> 
                  if biasedCoin 0.5 
                  then xv else yv) x.Pattern y.Pattern

         let singlePointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt = Utility.dice x.Size
            Array.mapi2 (fun i xv yv -> if i < pt then xv else yv) x.Pattern y.Pattern
         
         let twoPointCrossover (x:TernaryCondition) (y:TernaryCondition) = 
            let pt1,pt2 = Utility.dice x.Size, Utility.dice x.Size
            let p1,p2 = if pt1 = pt2 then pt1, min (pt2 + 2 ) x.Size else pt1,pt2
            let pp1,pp2 = if p1 > p2 then p2,p1 else p1, p2
            Array.mapi2 (fun i xv yv -> if i > pp1 && i < pp2 then yv else xv) x.Pattern y.Pattern
            
         let pat = 
            match crossoverType with 
            | Uniform ->  uniformCrossover this y
            | OnePoint -> singlePointCrossover this y
            | TwoPoint -> twoPointCrossover this y

         (new TernaryCondition(TrinaryPattern(pat))) 

      member x.SetStringValue (str:string) = pattern <- TrinaryPattern(Array.map (fun ch -> valueOfChar ch) (str.ToCharArray()))
 
      member x.StringValue() = pattern.ToString()

      member x.SubsumedBy (y:TernaryCondition) = 
         assert (x.Size = y.Size)
         x.Matches y && y.IsMoreGeneralThan x

      member x.IsMoreGeneralThan (y:TernaryCondition) = 
         assert (x.Size = y.Size)
         Array.forall2 (fun xv yv -> xv = DontCare || xv = yv) x.Pattern y.Pattern

      static member Cover ((pattern:BinaryPattern), prob) = 
         let tpattern = TrinaryPattern.Cover pattern prob
         new TernaryCondition(tpattern)
            