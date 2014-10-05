namespace BamaLlama.XCS

module Values = 

   type classifierValue = One | Zero | DontCare
   with 
      member x.isDontCare = 
         match x with 
         | DontCare -> true
         | _ -> false
      
      //TODO Can this be done more efficeintly ?
      member x.Eq y = 
         match x,y with 
         | (Zero,Zero) -> true
         | (One,One) -> true
         | (DontCare, DontCare) -> true
         |  _,_ -> false

      member x.Match y = 
         match x,y with 
         | _, DontCare -> true
         | DontCare, _ -> true
         | One, One -> true
         | Zero , Zero -> true
         | _,_ -> false
      
      static member Random flag =          
         match if flag then Utility.dice 3 else Utility.dice 2 with 
         | 1 -> Zero
         | 2 -> One
         | 3 -> DontCare
       
       static member Random() = classifierValue.Random true
       static member OfChar c = match c with | '0' -> Zero | '1' -> One | '#' -> DontCare | _ -> failwith "Invalid classifierValue character" 
       member x.RandomOther flag =  
          if flag 
          then 
              match x with 
              | One -> if Utility.coinToss() then Zero else DontCare
              | Zero -> if Utility.coinToss() then One else DontCare
              | DontCare -> if Utility.coinToss() then Zero else One
          else
             match x with 
             | Zero -> One
             | One -> Zero
          
       override x.ToString() = 
          match x with 
          | DontCare -> "#"
          | One -> "1"
          | Zero -> "0"  
                   
   type patternValue = One | Zero
   with 
      static member Random() = if Utility.coinToss() then Zero else One

   let matchValue (x:classifierValue) (y:patternValue) = 
      match x with 
      | DontCare -> true 
      | _ -> match (x , y) with 
             | classifierValue.One, patternValue.One 
             | classifierValue.Zero, patternValue.Zero -> true
             | _ -> false

   let valueOfChar ch = 
      if ch = '1' then classifierValue.One
      elif ch = '0' then classifierValue.Zero
      else DontCare

   let charOfValue v = 
      match v with 
      | classifierValue.One -> '1'
      | classifierValue.Zero -> '0'
      | DontCare -> '#'

   let classifierValueOf p = 
      match p with 
      | patternValue.One  -> classifierValue.One
      | patternValue.Zero -> classifierValue.Zero

   let mutate1 (x:classifierValue) (y:patternValue) flag = 
      if x.isDontCare then classifierValueOf y else (if flag then DontCare else x)


