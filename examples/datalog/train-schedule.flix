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
/// We then compute the detailed itinerary for the earliest connection.
///
def main(): Unit \ IO = run {
    let src = "Aalborg";
    let dst = "Copenhagen";
    let db = database();

    // A Datalog value that computes reachability subject to time constraints.
    let pr = #{
        // An edge is of the form (FromStation, ToStation, Train, DepartureTime, ArrivalTime).
        Path(x, y, tx, ty) :- Edge(x, y, _, tx, ty).
        Path(x, z, tx, tz) :- Path(x, y, tx, ty1), Edge(y, z, _, ty2, tz), if (ty1 < ty2).
    };

    // Use the database and Datalog program to compute *all* connections between `src` and `dst`.
    let connections = query db, pr select (src, dst, t1, t2) from Path(src, dst, t1, t2);

    if (Vector.isEmpty(connections)) {
        println("No connections found. Sorry.")
    } else {
        println("The available connections are:");
        foreach ((x, y, t1, t2) <- connections) {
            let (h, m) = duration(t1, t2);
            println("  ${fmt(t1)} ${x} ~~> ${y} ${fmt(t2)} [${h} hrs, ${m} min]")
        };
        let msg = "Choose a connection:";
        let i = Console.promptInt32(start = 0, end = Vector.length(connections), msg);
        let (_, _, t1, t2) = Vector.get(i, connections);

        // Use a provenance query to compute the edges that connect `src` and `dst`.
        let p = pquery db, pr select Path(src, dst, t1, t2) with {Edge};
        let itinerary = Vector.map(v -> ematch v { case Edge(s, d, train, tx, ty) => (s, d, train, tx, ty) }, p);

        println("\nThe itinerary for the chosen connection is:");
        foreach ((s, d, t, tx, ty) <- itinerary) {
            let fmtS = String.padRight(10, ' ', s);
            let fmtT = String.center(16, ' ', t);
            let fmtD = String.padRight(10, ' ', d);
            println("  ${fmt(tx)} ${fmtS} -- ${fmtT} --> ${fmtD} ${fmt(ty)}")
        }
    }} with Console.runWithIO

///
/// Returns the database of scheduled train departures and arrivals.
///
/// An edge is of the form (FromStation, ToStation, Train, DepartureTime, ArrivalTime).
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
/// Returns the duration between the two given times `t1` and `t2`.
///
/// For example, the duration between `(10,45)` and `(11,50)` is `(1, 5)`.
/// For example, the duration between `(10,45)` and `(11,30)` is `(0, 45)`.
///
def duration(t1: Time, t2: Time): Time = {
    let (h1, m1) = t1;
    let (h2, m2) = t2;
    if (m2 > m1) (h2 - h1, m2 - m1) else ((h2 - h1) - 1, (60 + m2) - m1)
}

///
/// Returns a human-readable representation of the given time `t`.
///
/// For example, `(10, 52)` is formatted as `(10.52)`.
/// For example, `(9, 7)`   is formatted as `(09.07)`.
///
def fmt(t: Time): String = {
    def f(x) = if (x <= 9) "0${x}" else "${x}";
    "(${f(fst(t))}.${f(snd(t))})"
}
