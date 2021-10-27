def drivable(g: #{Road(City, Int, City)}, src: City, dst: City, minSpeed: Int): Bool =
    let p = #{
        Path(x, y) :- Road(x, maxSpeed, y), if maxSpeed > minSpeed.
        Path(x, z) :- Path(x, y), Road(y, maxSpeed, z), if maxSpeed > minSpeed.
    };
    (solve g <+> p) |= Path(src, dst).
