def pipe(f: Int32 -> Int32 \ ef1, g: Int32 -> Int32 \ ef2): Int32 =
    List.range(1, 10)
        |> List.map(f)
        |> List.map(g)
        |> List.length
