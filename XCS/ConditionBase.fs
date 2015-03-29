namespace BamaLlama.XCS

open System
open System.IO
open Microsoft.FSharp.Core.Operators
open Base
open Values
module ConditionBase =   
   
   
   type recombinationType = Uniform | OnePoint | TwoPoint
   let recombinationTypeFromInt i = 
      match i with 
      | 0 -> Uniform
      | 1 -> OnePoint
      | 2 -> TwoPoint
      | _ -> failwith "Unknown Recombination type"

   [<AbstractClass>]
   type BasePattern(size) = 
      let pattern = Array.create size patternValue.Zero
      abstract member Length : int32
      default x.Length = size
      abstract member Pattern : patternValue array
      default x.Pattern = pattern

   
//   [<AbstractClass>]      
//   type public  CondBase() = 
//      static let mutable classData = ClassData.NewClassData "CondBase" "condition:base"     
//     
//      member x.ClassName = classData.ClassName
//      member x.TagName = classData.TagName
//      member x.ClassData with get() = classData and set(v) = classData <- v
//
//      abstract member Size : int
//      default x.Size = 0
//
//      abstract member AllowGaSubsumption : bool
//      default x.AllowGaSubsumption = false
//
//      abstract member AllowAsSubsumption : bool
//      default x.AllowAsSubsumption = false
//
//      abstract member Specificness : int
//      default  x.Specificness = x.Size
//
//      abstract member Generality : double
//      default x.Generality = 1.0 - x.Specificity
//
//      abstract member Specificity : double
//      default x.Specificity = (double)x.Specificness / (double)x.Size      
//      
//      abstract member SetStringValue : string -> unit
//      default x.SetStringValue s = ()
//
//      abstract member Match : BasePattern -> bool
//      default x.Match p = false
//
//      abstract member SubsumedBy : CondBase -> bool
//      default x.SubsumedBy(v) = false
//
//      abstract member IsMoreGeneralThan : CondBase -> bool
//      default x.IsMoreGeneralThan(v) = false    
//
//      abstract member StringValue : unit -> string;
//      default x.StringValue() = ""
//
//      abstract member Print : Stream -> unit
//      default x.Print str = 
//         use writer = new StreamWriter(str) 
//         writer.WriteLine(x.StringValue())
//      
                 
      

   