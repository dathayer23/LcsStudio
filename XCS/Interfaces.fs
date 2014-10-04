namespace BamaLlama.XCS

open System
open System.IO

module Interfaces = 
   type coverType = Standard | ActionBased
   type mutationType = Uniform | ThreeValue | TwoValue
   type recombinationType = Uniform | SinglePoint | DoublePoint 

   type IPattern<'v> =
      abstract Size : uint32
      abstract Pattern : 'v array 
      
      
   type IRandomizable<'a> = 
      abstract Random : unit ->  'a
      

   type IDiscovery<'a> = 
      abstract Mutate : double -> 'a
      abstract Mutate : mutationType * double -> 'a
      abstract Recombination : recombinationType -> 'a -> 'a
      abstract Cover : coverType -> IPattern<'v> -> 'a

   type ICondCompare<'a> = 
      abstract Specificness : uint32
      abstract Genricity : double
      abstract Specificity : double
      abstract IsSubsumedBy : 'a -> bool
      abstract IsMoreGeneralThan : 'a -> bool

   type IMatches<'a,'v> =
      abstract Match : IPattern<'v> -> bool
      abstract Matches : 'a -> bool

   type IStreaming<'a> = 
      abstract ToString : unit -> string
      abstract WriteToStream : Stream -> Stream
