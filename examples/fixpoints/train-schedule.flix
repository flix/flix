type alias Hour = Int32
type alias Minute = Int32
type alias Time = (Hour, Minute)
type alias Train = String
type alias Origin = String
type alias Destination = String

def main(): Unit \ IO =
    let db = database();
    let pr = program();
    foreach((x, y) <- query db, pr select (x, y) from Path(x, y)) {
        println("Path(${x}, ${y})")
    };
    let r = pquery db, pr select Path("Aarhus", "Copenhagen") with {Edge};
    Vector.forEach(v -> ematch v {
        case Edge(src, dst, _train, _departure, _arrival) => println("${src} -> ${dst}")
    }, r)

///
/// Returns a Datalog program that computes reachability subject to time constraints.
///
def program(): #{ Edge(Origin, Destination, Train, Time, Time), Path(Origin, Destination) | r } = #{
    Path(x, y) :- Edge(x, y, _, _, _).
    Path(x, z) :- Path(x, y), Edge(y, z, _, _, _).
}

///
/// Returns the database of scheduled trains.
///
def database(): #{ Edge(Origin, Destination, Train, Time, Time) | r } = #{
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
