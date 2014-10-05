namespace BamaLlama.XCS

open System
open System.IO

module Params = 
   exception ParameterException of string
   let IsInt str = Int32.TryParse(str) |> fst
   let IsDouble str = Double.TryParse(str) |> fst
   let ToInt str = Int32.TryParse(str) |> snd
   let ToDouble str = Double.TryParse(str) |> snd

   type param = Dbl of double | Int of int | Bl of bool | Str of string | Error of string
   with 
      member x.Double =  match x with Dbl v -> v | _ -> raise (ParameterException "Parameter is not a double type")
      member x.Integer = match x with Int v -> v | _ -> raise (ParameterException "Parameter is not an integer type")
      member x.Bool = match x with Bl v -> v | _ -> raise (ParameterException "Parameter is not a boolean type")
      member x.String = match x with Str v -> v | _ -> raise (ParameterException "Parameter is not a string type")
      static member FromString (str:string) = 
         match str.ToLower() with
         | "true" -> Bl(true)
         | "false" -> Bl(false)
         | _ when IsDouble(str) -> Dbl(ToDouble(str))
         | _ when IsInt(str) -> Int(ToInt(str))
         | _ -> Str(str)

   type namedParam = string * param
   
   let NamedParamFromString (str:string) = 
         let tokens = str.Split([|'='|]) |> Array.map (fun (s:string) -> s.Trim())
         if Array.length tokens = 2
         then ((tokens.[0], param.FromString(tokens.[1])))
         else ("Error", Error(sprintf "invalid string: [%s]" str))

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
      member x.ReadFromFile (f:string) = 
         use fs = new StreamReader(new FileStream(f, FileMode.Open, FileAccess.Read))
         let lines = fs.ReadToEnd().Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
         do parameters <- lines |> Array.toList 
                                |> List.map (fun (l:string) -> NamedParamFromString l)
                                

         