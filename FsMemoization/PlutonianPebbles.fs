module FsMemoization.PlutonianPebbles

open System.Collections.Generic

let private transform (stone: int64) =
    if stone = 0 then
        [1L]
    else
        let stoneString = string stone
        if stoneString.Length % 2 = 0 then
            let halfLength = stoneString.Length / 2
            [ int64 (stoneString.Substring(0, halfLength))
              int64 (stoneString.Substring(halfLength)) ]
        else
            [stone * 2024L]

let rec simpleStoneCountAfterBlinking blinks stone =
    let newStones = transform stone
    if blinks = 1 then
        int64 newStones.Length
    else
        newStones
        |> List.sumBy (simpleStoneCountAfterBlinking (blinks - 1))

let stoneCountAfterBlinkingForMemoization recursiveFunction (blinks, stone) =
    let newStones = transform stone
    if blinks = 1 then
        int64 newStones.Length
    else
        newStones
        |> List.sumBy (fun s -> recursiveFunction (blinks - 1, s))

let memoizeWithDictionary f =
    let cache = Dictionary<_,_>()
    let rec memoized param =
        match cache.TryGetValue param with
        | true, cachedValue -> cachedValue
        | false, _ ->
            let result = f memoized param
            cache.Add (param, result)
            result
    memoized

let memoizeWithMap f =
    let mutable cache = Map.empty
    let rec memoized param =
        match cache.TryFind param with
        | Some cachedValue -> cachedValue
        | None ->
            let result = f memoized param
            cache <- cache.Add(param, result)
            result
    memoized

let rec stoneCountAfterBlinkingWithCache cache (blinks, stone) =
    let cacheKey = (blinks, stone)
    match cache |> Map.tryFind cacheKey with
    | Some cachedResult -> cachedResult, cache
    | None ->
        let newStones = transform stone
        let result, updatedCache =
            if blinks = 1 then
                int64 newStones.Length, cache
            else
                let stoneCounts, cacheFromFold =
                    newStones
                    |> List.map (fun s -> blinks - 1, s)
                    |> List.mapFold stoneCountAfterBlinkingWithCache cache
                List.sum stoneCounts, cacheFromFold
        result, updatedCache.Add (cacheKey, result)