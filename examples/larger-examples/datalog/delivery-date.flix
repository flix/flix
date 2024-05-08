/// An example using Datalog constraints enriched with lattice semantics to
/// compute the delivery date of a part based on delivery dates of its components.
def main(): Unit \ IO =
    let p = #{
        // Parts and the components they depend on.
        PartDepends("Car",       "Chassis").
        PartDepends("Car",       "Engine").
        PartDepends("Engine",    "Piston").
        PartDepends("Engine",    "Ignition").

        // The time required to assemble a part from its components.
        AssemblyTime("Car",     7).
        AssemblyTime("Engine",  2).

        // The expected delivery date for certain components.
        DeliveryDate("Chassis";  2).
        DeliveryDate("Piston";   1).
        DeliveryDate("Ignition"; 7).

        // A part is ready when it is delivered.
        ReadyDate(part; date) :-
            DeliveryDate(part; date).

        // Or when it can be assembled from its components.
        ReadyDate(part; assemblyTime + componentDate) :-
            PartDepends(part, component),
            AssemblyTime(part, assemblyTime),
            ReadyDate(component; componentDate).
    };

    // Computes a map from parts to delivery dates.
    let m = query p select (c, d) from ReadyDate(c; d) |> Vector.toMap;

    // Looks up the delivery date for the car and prints it.
    Map.getWithDefault("Car", 0, m) |> println
