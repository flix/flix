pub def travelWithLimitedBusses(trainConnections: List[(city, city)], busConnections: List[(city, city)], source: city, destination: city, maxBusTrips: Int32): Bool with Order[city] =
    let tc = inject trainConnections into Train;
    let bc = inject busConnections into Bus;
    let lp = #{
        Path(x, 0, y) :- Train(x, y).
        Path(x, busses, z) :- Path(x, busses, y), Train(y, z).
        Path(x, 1, y) :- Bus(x, y).
        Path(x, busses + 1, z) :- Path(x, busses, y), Bus(y, z).
    };
    let possibleBusTrips = query tc, bc, lp select busses from Path(source, busses, destination) where busses <= maxBusTrips;
    List.length(possibleBusTrips) > 0
