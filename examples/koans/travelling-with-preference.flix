pub def travelWithLimitedBusses(trainConnections: Array[(city, city)], busConnections: Array[(city, city)], source: city, destination: city, maxBusTrips: Int): Bool with Boxable[city] =
    let tc = project trainConnections into Train;
    let bc = project busConnections into Bus;
    let lp = #{
        Path(x, 0, y) :- Train(x, y).
        Path(x, busses, z) :- Path(x, busses, y), Train(y, z).
        Path(x, 1, y) :- Bus(x, y).
        Path(x, busses + 1, z) :- Path(x, busses, y), Bus(y, z).
    };
    let possibleBusTrips = query tc, bc, lp select busses from Path(source, busses, destination) where busses <= maxBusTrips;
    possibleBusTrips.length > 0
