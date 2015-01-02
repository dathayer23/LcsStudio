namespace BamaLlama.XCS
open Params
module Base = 
   type ClassData = { className : string; tagName : string ; mutable parameters : Parameters }
   with   
      member x.SetParameters (pms: ParameterDB) =  x.parameters <- pms.GetSubject(x.tagName)
      member x.ClassName : string = x.className
      member x.TagName : string = x.tagName
      static member NewClassData className tagName = 
         { 
            className = className; 
            tagName = tagName; 
            parameters = new Parameters([]) 
         }
