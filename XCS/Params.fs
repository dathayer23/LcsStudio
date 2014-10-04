namespace BamaLlama.XCS


module Params = 
   exception ParameterException of string
   type param = Dbl of double | Int of int | Bl of bool | Str of string
   with 
      member x.Double =  match x with Dbl v -> v | _ -> raise (ParameterException "Parameter is not a double type")
      member x.Integer = match x with Int v -> v | _ -> raise (ParameterException "Parameter is not an integer type")
      member x.Bool = match x with Bl v -> v | _ -> raise (ParameterException "Parameter is not a boolean type")
      member x.String = match x with Str v -> v | _ -> raise (ParameterException "Parameter is not a string type")

   type namedParam = string * param
   type Parameters() = 
      let mutable parameters: namedParam list = []

      member x.Exists name = 
         match List.tryFind (fun (nm, pm) -> nm = name ) parameters with 
         | Some _ -> true
         | None -> false

      member x.Add name param = parameters <- (name, param) :: parameters
      member x.Remove name = parameters <- List.filter (fun (n,_) -> n = name) parameters
      member x.Retrieve name = 
         match List.tryFind (fun (nm, pm) -> nm = name ) parameters with 
         | Some (nm,p) -> p
         | None -> raise ( ParameterException (sprintf "Parameter named %s not found"  name))
      member x.Modify nm pm = 
         do x.Remove nm
         x.Add nm pm

      member x.Item with get nm = x.Retrieve nm and set nm pm = x.Modify nm pm
      member x.TryGetDouble nm def = if x.Exists nm then x.[nm].Double else def
      member x.TryGetInteger nm def = if x.Exists nm then x.[nm].Integer else def
      member x.TryGetBool nm def = if x.Exists nm then x.[nm].Bool else def
      member x.TryGetString nm def = if x.Exists nm then x.[nm].String else def