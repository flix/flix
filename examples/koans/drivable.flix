pub def drivable(roads: List[(city, city)], source: city, destination: city): Bool with Boxable[city] =
    let r = inject roads into Road;
    let lp = #{
        Path(x, y) :- Road(x, y).
        Path(x, z) :- Path(x, y), Road(y, z).
    };
    not Array.isEmpty(query r, lp select () from Path(source, destination))
