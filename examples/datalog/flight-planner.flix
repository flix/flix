pub def main(_args: Array[String]): Int & Impure =
    f("Denmark", "Cairo")
        |> List.sortBy(r -> r.flights)
        |> List.map(r -> "flights: ${r.flights}, time: ${r.time}")
        |> List.foreach(println);
    println("test");
    0

def f(currentCity: String, targetCity: String): List[({flights::Int, time::Int})] & Impure =
    let flights = #{
        Flight("Denmark", 12, "Cairo").
        Flight("Denmark", 2, "Spain").
        Flight("Spain", 9, "Cairo").
    };
    let prog = #{
        Trip(0, currentCity; 0).
        Trip(leg+1, destination; time + duration) :- Trip(leg, departure; time), Flight(departure, duration, destination), if leg < 10.
    };
    query flights, prog select (leg, time) from Trip(leg, targetCity; time) |> Array.map(match (l, t) -> {flights=l, time=t}) |> Array.toList
