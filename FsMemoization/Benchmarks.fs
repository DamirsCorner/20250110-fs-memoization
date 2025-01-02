namespace FsMemoization

open BenchmarkDotNet.Attributes
open FsMemoization.PlutonianPebbles

type Benchmarks() =
    
    [<Params(25, 30, 35)>]
    member val blinks = 0 with get, set
    
    member val stones = [ 125L; 17L ] with get
    
    [<Benchmark(Baseline = true)>]
    member this.Simple () =
        this.stones
        |> List.sumBy (simpleStoneCountAfterBlinking this.blinks)
    
    [<Benchmark>]
    member this.Dictionary () =
        let memoized = memoizeWithDictionary(stoneCountAfterBlinkingForMemoization)
        this.stones
        |> List.sumBy (fun stone -> memoized (this.blinks, stone))
    
    [<Benchmark>]
    member this.MutableMap () =
        let memoized = memoizeWithMap(stoneCountAfterBlinkingForMemoization)
        this.stones
        |> List.sumBy (fun stone -> memoized (this.blinks, stone))
    
    [<Benchmark>]
    member this.MapState () =
        this.stones
        |> List.map (fun stone -> this.blinks, stone) 
        |> List.mapFold stoneCountAfterBlinkingWithCache Map.empty
        |> fst
        |> List.sum
