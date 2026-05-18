def pipe(): Int32 =
    List.range(1, 10)
        |> List.map(x -> x + 1)
        |> List.map(println)
        |> List.length
