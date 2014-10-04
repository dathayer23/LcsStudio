namespace BamaLlama.XcsTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open BamaLlama.XCS.Params
open BamaLlama.XCS.TernaryCondition

[<TestClass>]
type UnitTest() = 
    [<TestMethod>]
    member x.``Set String Value Returns Same String`` () = 
      let pat = Array.init 15 (fun _ -> classifierValue.Random true)
      let string = Array.map(fun v -> charOfValue v) pat |> (fun (chs : char []) -> new String(chs))
      let testVal = new TernaryCondition(pat, new Parameters())
      let testString = testVal.StringValue()
      Assert.AreEqual(string, testString)
