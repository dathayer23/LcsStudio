﻿namespace BamaLlama.XCS

open System
open System.Diagnostics
open System.IO
open Params

module Program = 
   let mutable params: ParameterDB = null
   let mutable ext : string = ""

   let ProcessArguments args = 
      do ext <- "txt"

   
   let ConfigureExperiment() = 
      do Console.WriteLine("\nSystem Start ...")
      let file = File.Open(sprintf "config.%s" ext, FileMode.Open)
      do params <- Params.ReadParams(file)

   let PerformExperiment() = ()
   let ProcessStats() = ()
      
   [<EntryPoint>]
   let main argv = 
      if argv.Length = 0 
      then
         do Console.WriteLine("USAGE:\t\t XCS-System -f <suffix> [-v] [ -s <set>]")
         do Console.WriteLine("\t\t\t\t<suffix>   suffix for the configuration file")
         do Console.WriteLine("\t\t\t\t-v         verbose output")
         do Console.WriteLine("\t\t\t\t-h         print version")
         
      else
         do ProcessArguments argv
         do ConfigureExperiment()
         do PerformExperiment()
      
      do ProcessStats()  
      0
