///
/// We introduce a collection of type aliases to model the train domain.
///
type alias Station = String             // e.g. Aarhus
type alias Train   = String             // e.g. InterCity-432.
type alias Time    = (Int32, Int32)     // e.g. (10, 32) which is 10.32 AM.

def main(): Unit \ IO =
    let src = "Aalborg";
    let dst = "Copenhagen";
    let schedules = findSchedules(src, dst);
    if (Vector.isEmpty(schedules)) {
        println("No connections found. Sorry.")
    } else {
        println("Available connections:");
        foreach ((x, y, t) <- schedules) {
            println("  ${x} to ${y} arrival at ${t}")
        };
        println("The trip for the earliest arrival is:");
        let (_, _, t) = Vector.get(0, schedules);
        tripOf(src, dst, t)
    }

///
/// Returns all schedules from the given origin to the given destination.
///
def findSchedules(origin: Station, dest: Station): Vector[(Station, Station, Time)] =
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
            let tr = String.padRight(20, ' ', "-- ${train} -->");
            let d = String.padRight(10, ' ', dst);
            println("  ${fmt(t1)} ${s} ${tr} ${d} ${fmt(t2)}")
    }, r)
}

///
/// Returns a Datalog program that computes reachability subject to time constraints.
///
def program(): #{ Edge(Station, Station, Train, Time, Time), Path(Station, Station, Time) | r } = #{
    Path(x, y, t)  :- Edge(x, y, _, _, t).
    Path(x, z, t2) :- Path(x, y, t0), Edge(y, z, _, t1, t2), if (t0 < t1).
}

///
/// Returns the database of scheduled trains.
///
def database(): #{ Edge(Station, Station, Train, Time, Time) | r } = #{
    // Aalborg --> Aarhus
    Edge("Aalborg", "Aarhus", "IC-4332", (09, 09), (10, 40)).
    Edge("Aalborg", "Aarhus", "IL-4034", (09, 59), (11, 08)).
    Edge("Aalborg", "Aarhus", "IC-4336", (10, 07), (11, 38)).

    // Aarhus -> Kolding
    Edge("Aarhus", "Kolding", "IC-432",  (10, 45), (12, 04)).
    Edge("Aarhus", "Kolding", "IC-436",  (11, 45), (13, 04)).

    // Kolding -> Odense
    Edge("Kolding", "Odense", "IL-934",  (12, 05), (12, 54)).
    Edge("Kolding", "Odense", "IC-838",  (13, 13), (13, 52)).
    Edge("Kolding", "Odense", "EuroCity-396", (13, 46), (14, 23)).

    // Odense -> Copenhagen
    Edge("Odense", "Copenhagen", "IL-38",  (13, 57), (15, 06)).
    Edge("Odense", "Copenhagen", "IL-42",  (14, 57), (16, 06)).
}

def fmt(t: Time): String = {
    def f(x) = if (x < 9) "0${x}" else "${x}";
    "(${f(fst(t))}.${f(snd(t))})"
}
