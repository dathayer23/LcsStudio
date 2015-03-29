// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace BamaLlama.XCS
open System.IO
open BamaLlama.XCS.Params
open BamaLlama.XCS.SystemManager

module XcsRunner = 
   [<EntryPoint>]
   let main argv = 
       let fs = new FileStream(@"..\..\confsys.mp6", FileMode.Open, FileAccess.Read)
       let paramDb =  Params.ReadParams fs
       let experiment = new LcsManager(paramDb)
       experiment.PerformExperiments()
       printfn "%A" argv
       0 // return an integer exit code
