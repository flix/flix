pub def drivable(roads: List[(city, city)], source: city, destination: city): Bool with Order[city] =
    let r = inject roads into Road;
    let lp = #{
        Path(x, y) :- Road(x, y).
        Path(x, z) :- Path(x, y), Road(y, z).
    };
    not Vector.isEmpty(query r, lp select () from Path(source, destination))
