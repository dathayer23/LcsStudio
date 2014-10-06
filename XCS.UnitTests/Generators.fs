
namespace BamaLlama.XcsTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open BamaLlama.XCS.Values
open BamaLlama.XCS.TernaryCondition

module Generators = 
   
   let genBinaryValue = Gen.oneof [gen { return patternValue.Zero }; gen{ return patternValue.One}]
   let genBinaryChar  = Gen.oneof [gen { return '0' }; gen{ return '1' }]
   let genTrinaryValue (prob:double) = 
      let dontCare = (int)(100.0 * prob)
      let otherChar = (100 - dontCare)/2
      Gen.frequency [ 
         (dontCare , gen { return classifierValue.DontCare});  
         (otherChar, gen{ return classifierValue.Zero})
         (otherChar, gen{ return classifierValue.One})
         ]

   let genTrinaryChar (prob:double) = 
      let dontCare = (int)(100.0 * prob)
      let otherChar = (100 - dontCare)/2
      Gen.frequency [ 
         (dontCare , gen { return '?'});  
         (otherChar, gen{ return '0'})
         (otherChar, gen{ return '1'})
         ]

   let genTrinaryArray (gen: double -> Gen<_>) (prob:double) (s:int) = Gen.arrayOfLength s (gen prob) 
      
   let genTrinaryValues =  genTrinaryArray genTrinaryValue 
   let genTrinaryChars = fun d s -> Gen.map (fun (cs:char[]) -> new String(cs)) (genTrinaryArray genTrinaryChar d s) 
   let genTrinaryPatterns = fun d s -> Gen.map (fun (cs:classifierValue[]) -> new TrinaryPattern(cs)) (genTrinaryArray genTrinaryValue d s) 

   type MyGenerators =
     static member patternValue() = 
         {
           new Arbitrary<patternValue>() with
             override x.Generator = genBinaryValue
             override x.Shrinker t = Seq.empty
         }

     static member classifierValue() =
         {
            new Arbitrary<classifierValue>() with
             override x.Generator = (genTrinaryValue 0.2)
             override x.Shrinker t = Seq.empty
         }

     static member TrinaryPattern() =
         {
            new Arbitrary<TrinaryPattern>() with
             override x.Generator = (genTrinaryPatterns 0.2 100)
             override x.Shrinker t = Seq.empty
         }

   do Arb.register<MyGenerators>() |> ignore