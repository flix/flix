def pipe(f: Int32 -> Int32 \ ef): Int32 =
    List.range(1, 10)
        |> List.map(f)
        |> List.length
