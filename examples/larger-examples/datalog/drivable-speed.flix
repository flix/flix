pub def drivable(roads: List[(city, Int32, city)], source: city, destination: city, minimumSpeed: Int32): Bool with Order[city] =
    let r = inject roads into Road;
    let lp = #{
        Path(x, y) :- Road(x, maximumSpeed, y), if maximumSpeed > minimumSpeed.
        Path(x, z) :- Path(x, y), Road(y, maximumSpeed, z), if maximumSpeed > minimumSpeed.
    };
    not Vector.isEmpty(query r, lp select () from Path(source, destination))
