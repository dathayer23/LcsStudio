namespace BamaLlama.XcsTests

open System
open System.Diagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open BamaLlama.XCS.Params
open BamaLlama.XCS.TernaryCondition
open BamaLlama.XCS.Values
open Generators

[<TestClass>]
type UnitTest() = 

    [<TestMethod>]
    member __.``Set String Value Returns Same String from TernaryCondition`` () = 
      let ClassifierValuesReturnIdenticalStringFromTernaryCondition(pat:classifierValue[]) =
         let str = System.String(Array.map(fun v -> charOfValue v) pat)
         let testVal = new TernaryCondition(TrinaryPattern(pat), Parameters())
         let testString = testVal.StringValue()
         Assert.AreEqual(str, testString)
      Check.VerboseThrowOnFailure ClassifierValuesReturnIdenticalStringFromTernaryCondition

    [<TestMethod>]
    member __.``Generic String IO Roundtrip is Identity``() = 
       let StringRoundtrip (tripat:TrinaryPattern) = 
          do Debug.WriteLine(tripat.ToString())
          let pattern = tripat.ToString()
          let cond = new TernaryCondition(pattern, Parameters())
          let pattern2 = cond.StringValue()
          Assert.AreEqual(pattern2, pattern)

       Check.VerboseThrowOnFailure StringRoundtrip

    [<TestMethod>]
    member __.``Randomized Ternary Pattern has same size as original``() =
       let RandomPatternHasSameSize (tripat:TrinaryPattern) = 
          let pattern2 = tripat.Random()
          Assert.AreEqual(tripat.Size, pattern2.Size)

       Check.VerboseThrowOnFailure RandomPatternHasSameSize

    [<TestMethod>]
    member __.``Randomized Ternary Condition has same size as original``() =
       let RandomConditionHasSameSize (tripat:TrinaryPattern) = 
          let cond = new TernaryCondition(tripat, Parameters())

          let pattern2 = cond.Random()
          Assert.AreEqual(cond.Size, pattern2.Size)

       Check.VerboseThrowOnFailure RandomConditionHasSameSize

    [<TestMethod>]
    member __.``Covering Classifier Matches Pattern``() =
      let CoverMatchesPattern(pattern:BinaryPattern) =
         let cond = new TernaryCondition(pattern.Size)
         let cover = cond.Cover pattern
         (cover.Match pattern)

      Check.VerboseThrowOnFailure CoverMatchesPattern