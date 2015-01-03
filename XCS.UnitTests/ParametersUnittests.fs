namespace BamaLlama.XcsTests

open System
open System.Diagnostics
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open BamaLlama.XCS.Params



[<TestClass>]
type UnitTest() = 
      
   [<TestMethod>]
   member __.``Read parameter file into parameater  database``() =
      use file = File.Open(@"C:\Users\dthayer\Documents\GitHub\LcsStudio\XCS_Lib\examples\woods2\confsys.woods2", FileMode.Open)
      
      //let parameterDb = BamaLlama.XCS.Params.ReadParams(file)
      
      Assert.IsTrue(true)
