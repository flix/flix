def drivable(g: #{Road(City, City)}, src: City, dst: City): Bool =
    let p = #{
        Path(x, y) :- Road(x, y).
        Path(x, z) :- Path(x, y), Road(y, z).
    };
    (solve g <+> p) |= Path(src, dst).
