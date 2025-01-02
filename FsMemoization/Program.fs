module FsMemoization.Program

open BenchmarkDotNet.Running

BenchmarkRunner.Run<Benchmarks>() |> ignore
