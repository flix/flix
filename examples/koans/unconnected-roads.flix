def unconnected(g: #{Road(City, City)}): #{Unconnected(City, City)} =
    let p = #{
        City(x) :- Road(x, _).
        City(y) :- Road(_, y).
        Path(x, y) :- Road(x, y).
        Path(x, z) :- Path(x, y), Road(y, z).
        Unconnected(x, y) :- City(x), City(y), not Path(x, y).
    };
    project Unconnected (solve (g <+> p))
