namespace BamaLlama.XcsTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open BamaLlama.XCS.Params
open BamaLlama.XCS.TernaryCondition
open BamaLlama.XCS.Values
[<TestClass>]
type UnitTest() = 
    [<TestMethod>]
    member x.``Set String Value Returns Same String`` () = 
      let pat = Array.init 15 (fun _ -> classifierValue.Random true)
      let string = Array.map(fun v -> charOfValue v) pat |> (fun (chs : char []) -> String(chs))
      let testVal = new TernaryCondition(TrinaryPattern(pat), Parameters())
      let testString = testVal.StringValue()
      Assert.AreEqual(string, testString)
