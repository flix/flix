def pipe(): Int32 =
    region rc {
        Array.range(rc, 0, 100)
        |> Array.toList
        |> List.map(x -> x + 1)
        |> List.map(println)
        |> List.length
    }
