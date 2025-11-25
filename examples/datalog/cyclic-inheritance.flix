///
/// We introduce type aliases to model the inheritance domain.
///
type alias Class = String         // e.g. "Circle"
type alias Interface = String     // e.g. "Shape"

///
/// We compute all inheritance cycles in a class hierarchy.
///
/// We then compute the inheritance path for each cycle found.
///
def main(): Unit \ IO =
    let db = database();

    // A Datalog program that computes the transitive supertype relation.
    let pr = #{
        // A supertype relationship can come from class extension or interface implementation.
        SuperType(x, y) :- Extends(x, y).
        SuperType(x, y) :- Implements(x, y).

        // Compute the transitive closure of supertype relation.
        SuperType(x, z) :- SuperType(x, y), Extends(y, z).
        SuperType(x, z) :- SuperType(x, y), Implements(y, z).

        // A cycle exists when a type is its own supertype.
        Cycle(x) :- SuperType(x, x).
    };

    // Compute all cycles in the class hierarchy.
    let cycles = query db, pr select x from Cycle(x);

    if (Vector.isEmpty(cycles)) {
        println("No inheritance cycles found. Class hierarchy is valid!")
    } else {
        println("ERROR: Inheritance cycles detected!\n");
        println("The following types have cyclic inheritance:");
        foreach (typ <- cycles) {
            println("  - ${typ}")
        };
        println("");

        foreach (typ <- cycles) {
            println("Cycle involving '${typ}':");

            // Use a provenance query to find the inheritance edges that form the cycle.
            let p = pquery db, pr select Cycle(typ) with {Extends, Implements};
            foreach (edge <- p) {
                ematch edge {
                    case Extends(src, dst) => {
                        let fmtSrc = String.padRight(15, ' ', src);
                        let fmtRel = String.center(12, ' ', "extends");
                        println("    ${fmtSrc} ${fmtRel} ${dst}")
                    }
                    case Implements(src, dst) => {
                        let fmtSrc = String.padRight(15, ' ', src);
                        let fmtRel = String.center(12, ' ', "implements");
                        println("    ${fmtSrc} ${fmtRel} ${dst}")
                    }
                }
            };
            println("")
        }
    }

///
/// Returns the database of class and interface relationships.
///
/// This models a geometric shape hierarchy with some intentional cycles
/// to demonstrate the cycle detection.
///
def database(): #{ Extends(Class, Class), Implements(Class, Interface) | r } = #{
    // Valid shape hierarchy
    Implements("Circle", "Shape").
    Implements("Rectangle", "Shape").
    Implements("Triangle", "Shape").

    Extends("Square", "Rectangle").
    Extends("Rhombus", "Quadrilateral").
    Extends("Quadrilateral", "Polygon").
    Implements("Polygon", "Shape").

    // Cycle 1: Circle -> Ellipse -> OvalShape -> Circle
    Extends("Circle", "Ellipse").
    Extends("Ellipse", "OvalShape").
    Extends("OvalShape", "Circle").

    // Cycle 2: Drawable <-> Paintable interface mutual dependency
    Implements("Drawable", "Paintable").
    Implements("Paintable", "Drawable").
}

