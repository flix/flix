///
/// We introduce a collection of type aliases to model the train domain.
///
/// In the real-world we would use newtype wrappers for increased type safety.
///
type alias Station = String             // e.g. Aarhus
type alias Train   = String             // e.g. InterCity-432.
type alias Time    = (Int32, Int32)     // e.g. (10, 32) which is 10.32 AM.

///
/// We compute all connections from Aalborg to Copenhagen.
///
/// We also compute the detailed itinerary for one of the trips.
///
def main(): Unit \ IO =
    let src = "Aalborg";
    let dst = "Copenhagen";
    let connections = getConnections(src, dst);
    if (Vector.isEmpty(connections)) {
        println("No connections found. Sorry.")
    } else {
        println("The available connections are:");
        foreach ((x, y, t) <- connections) {
            println("  ${x} ~~> ${y} ${fmt(t)}")
        };
        println("\nThe itinary for the earliest connection is:\n");
        let (_, _, t) = Vector.get(0, connections);
        tripOf(src, dst, t)
    }

///
/// Returns all connections from the given origin to the given destination.
///
def getConnections(origin: Station, dest: Station): Vector[(Station, Station, Time)] =
    let db = database();
    let pr = program();
    query db, pr select (origin, dest, t) from Path(origin, dest, t)


def tripOf(origin: Station, dest: Station, t: Time): Unit \ IO = {
    let db = database();
    let pr = program();
    let r = pquery db, pr select Path(origin, dest, t) with {Edge};
    Vector.forEach(v -> ematch v {
        case Edge(src, dst, train, t1, t2) =>
            let s = String.padRight(10, ' ', src);
            let tr = String.center(16, ' ', "${train}");
            let d = String.padRight(10, ' ', dst);
            println("  ${fmt(t1)} ${s} -- ${tr} --> ${d} ${fmt(t2)}")
    }, r)
}

///
/// Returns the Datalog program that computes reachability subject to time constraints.
///
/// The Datalog program determines all pairs `(x, y)` where `y` is reachable from `x`
/// by one or more edges such that departure times are respected.
///
def program(): #{ Edge(Station, Station, Train, Time, Time), Path(Station, Station, Time) | r } = #{
    Path(x, y, t)  :- Edge(x, y, _, _, t).
    Path(x, z, t2) :- Path(x, y, t0), Edge(y, z, _, t1, t2), if (t0 < t1).
}

///
/// Returns the database of scheduled train departures and arrivals.
///
/// Each trip is represented as an edge:
///
///     Edge(FromStation, ToStation, Train, DepartureTime, ArrivalTime)
///
def database(): #{ Edge(Station, Station, Train, Time, Time) | r } = #{
    // Aalborg --> Aarhus
    Edge("Aalborg", "Aarhus", "InterCity 4332",     (09, 09), (10, 40)).
    Edge("Aalborg", "Aarhus", "InterCityLyn 4034",  (09, 59), (11, 08)).
    Edge("Aalborg", "Aarhus", "InterCity 4336",     (10, 07), (11, 38)).

    // Aarhus -> Kolding
    Edge("Aarhus", "Kolding", "InterCity 432",      (10, 45), (12, 04)).
    Edge("Aarhus", "Kolding", "InterCity 436",      (11, 45), (13, 04)).

    // Kolding -> Odense
    Edge("Kolding", "Odense", "InterCityLyn 934",   (12, 05), (12, 54)).
    Edge("Kolding", "Odense", "InterCity 838",      (13, 13), (13, 52)).
    Edge("Kolding", "Odense", "EuroCity-396",       (13, 46), (14, 23)).

    // Odense -> Copenhagen
    Edge("Odense", "Copenhagen", "InterCityLyn 38", (13, 57), (15, 06)).
    Edge("Odense", "Copenhagen", "InterCityLyn 42", (14, 57), (16, 06)).
}

///
/// Returns a human-readable representation of the given time `t`.
///
/// For example, `(10, 52)` is formatted as `(10.52)`.
/// For example, `(9, 7)`   is formatted as `(09.07)`.
///
def fmt(t: Time): String = {
    def f(x) = if (x < 9) "0${x}" else "${x}";
    "(${f(fst(t))}.${f(snd(t))})"
}
