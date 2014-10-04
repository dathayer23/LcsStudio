namespace BamaLlama.XCS

module Base = 
   type ClassData(cls,tag) = 
      let className = cls
      let tagName = tag

      member x.ClassName : string = className
      member x.TagName : string = tagName

