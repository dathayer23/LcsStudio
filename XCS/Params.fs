namespace BamaLlama.XCS

open System
open System.IO
open Microsoft.FSharp.Core

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
         | "on"
         | "true" -> Bl(true)
         | "off"
         | "false" -> Bl(false)

         | _ when str.Contains(".") && IsDouble(str) -> Dbl(ToDouble(str))
         | _ when IsInt(str) -> Int(ToInt(str))
         | _ -> Str(str)

   type NamedParam = { Name: string; Param : param }
   
   let NamedParamFromString (str:string) = 
         let tokens = str.Split([|'='|]) |> Array.map (fun (s:string) -> s.Trim())
         if Array.length tokens = 2
         then ({Name = tokens.[0]; Param =  param.FromString(tokens.[1])})
         else ({Name = "Error"; Param = Error(sprintf "invalid string: [%s]" str)})

   type Parameters(``params``) = 
      let mutable parameters: NamedParam list = ``params``
      new () = Parameters([])
      
      member x.Empty = parameters.Length = 0
      member x.Parameters = parameters
      member x.Exists name = 
         match List.tryFind (fun pm -> pm.Name = name ) parameters with 
         | Some _ -> true
         | None -> false

      member x.Add name param = parameters <- ({Name = name; Param = param}) :: parameters
      member x.Remove name = parameters <- List.filter (fun p -> p.Name = name) parameters
      member x.Retrieve name = 
         match List.tryFind (fun p -> p.Name = name ) parameters with 
         | Some pm -> pm.Param
         | None -> raise ( ParameterException (sprintf "Parameter named %s not found"  name))
      member x.Modify nm pm = 
         do x.Remove nm
         x.Add nm pm

      member x.Merge(prms:Parameters) = 
         let addParam (p:NamedParam) = 
            if x.Exists (p.Name) 
            then do x.Remove (p.Name)
            else ()
            do x.Add (p.Name) (p.Param)

         do List.map (fun p -> addParam p) prms.Parameters |> ignore
         x
               
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
                                

   [<AllowNullLiteral>]
   type ParameterDB (``params``)  =         
      let mutable parameters : Map<string, Parameters> = ``params``
      new () = ParameterDB(Map.empty<string, Parameters>)
      member x.GetSubject subj = match Map.tryFind subj parameters with Some lst -> lst | None -> new Parameters()
      member x.NewSubject subj prms = 
         match  Map.tryFind subj parameters with 
         | Some pms -> 
            let newMap = Map.remove subj parameters
            let newMap = Map.add subj (pms.Merge(prms)) newMap
            do parameters <- newMap
            ()

         | None -> do parameters <- Map.add subj prms parameters

      member x.AddParam subj param = x.NewSubject subj (new Parameters([param]))
            
      
   let ReadParams (file:FileStream) =
      use strm = new StreamReader(file)
      let s = strm.ReadToEnd() 
      let lines = s.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 
      let l2 = 
         lines
            |> Array.filter (fun (s:string) -> not (s.StartsWith("##")))
            |> List.ofArray

      let paramDb = new ParameterDB()

      let processLines lines = 
         let processLine subj (str:string) = 
            if str.StartsWith("<") || str.StartsWith("[")
            then 
               let newSubj = str.Trim([| '<'; '>'; '['; ']'; '/'; ' '; '\r'; '\n'; '\t'|])
               if newSubj = subj
               then ("", None)
               else (newSubj, None)
            else
               let pm = str.Trim([| '<'; '>'; '['; ']'; '/'; ' '; '\r'; '\n'; '\t'|])
               if String.IsNullOrEmpty(pm)
               then (subj, None)
               else (subj, Some(NamedParamFromString pm))

         let rec Process subj lines = 
            match lines with 
            | [] -> paramDb
            | l :: ll -> 
               match processLine subj l with 
               | subj, None -> Process subj ll
               | subj, Some prm -> 
                     do paramDb.AddParam subj prm
                     Process subj ll
                                         
         Process "" lines
      processLines l2
