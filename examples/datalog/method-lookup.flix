///
/// We introduce type aliases to model the class hierarchy.
///
type alias Class = String       // e.g. "Rectangle"
type alias Interface = String   // e.g. "Shape"
type alias Method = String      // e.g. "draw"

///
/// We compute all methods available on a type and use a provenance query
/// to compute from where the method is inherited.
///
pub def main(): Unit \ IO =
    let tt = "Square";
    let db = database();

    // A Datalog program that computes the supertype relation and all inherited methods.
    let pr = #{
        // A supertype relationship can come from class extension or interface implementation.
        SuperType(x, y) :- Extends(x, y).
        SuperType(x, y) :- Implements(x, y).

        // Compute the transitive closure of the supertype relation.
        SuperType(x, z) :- SuperType(x, y), Extends(y, z).
        SuperType(x, z) :- SuperType(x, y), Implements(y, z).

        // A method is available on a type if it is declared directly on that type.
        AvailableMethod(t, m) :- MethodDecl(t, m).

        // A method is available on a type if it is available on a supertype.
        AvailableMethod(t, m) :- SuperType(t, s), MethodDecl(superTyp, m).
    };

    // Query for all methods available on the target type `tt`.
    let methods = query db, pr select method from AvailableMethod(tt, method);
    println("Methods available on '${tt}':\n");

    foreach (m <- methods) {
        // Use a provenance query to find where method `m` was declared.
        let p = pquery db, pr select AvailableMethod(tt, m) with {MethodDecl};

        // Extract the declaring class from the provenance.
        foreach (decl <- p) {
            ematch decl {
                case MethodDecl(declaringClass, _) => {
                    let fmtMethod = String.padRight(15, ' ', m);
                    let source = if (declaringClass == tt) "self" else "inherited";
                    println("  ${fmtMethod} <- ${declaringClass} (${source})")
                }
            }
        }
    }


///
/// Returns the database of class relationships and method declarations.
///
/// This models a geometric shape hierarchy with realistic method definitions.
///
def database(): #{ Extends(Class, Class), Implements(Class, Interface), MethodDecl(Class, Method) | r } = #{
    // Class hierarchy
    Implements("Rectangle", "Shape").
    Implements("Circle", "Shape").
    Implements("Triangle", "Shape").
    Extends("Square", "Rectangle").

    Implements("Polygon", "Shape").
    Extends("Pentagon", "Polygon").
    Extends("Hexagon", "Polygon").

    // Methods on Shape interface
    MethodDecl("Shape", "draw").
    MethodDecl("Shape", "area").
    MethodDecl("Shape", "toString").

    // Methods on Rectangle
    MethodDecl("Rectangle", "draw").
    MethodDecl("Rectangle", "area").
    MethodDecl("Rectangle", "perimeter").
    MethodDecl("Rectangle", "width").
    MethodDecl("Rectangle", "height").
    MethodDecl("Rectangle", "isSquare").

    // Methods on Square (overrides some Rectangle methods)
    MethodDecl("Square", "draw").
    MethodDecl("Square", "side").

    // Methods on Circle
    MethodDecl("Circle", "draw").
    MethodDecl("Circle", "area").
    MethodDecl("Circle", "circumference").
    MethodDecl("Circle", "radius").
    MethodDecl("Circle", "diameter").

    // Methods on Triangle
    MethodDecl("Triangle", "draw").
    MethodDecl("Triangle", "area").
    MethodDecl("Triangle", "perimeter").
    MethodDecl("Triangle", "isEquilateral").

    // Methods on Polygon
    MethodDecl("Polygon", "draw").
    MethodDecl("Polygon", "area").
    MethodDecl("Polygon", "perimeter").
    MethodDecl("Polygon", "vertices").
    MethodDecl("Polygon", "edges").

    // Methods on Pentagon
    MethodDecl("Pentagon", "draw").

    // Methods on Hexagon
    MethodDecl("Hexagon", "draw").
    MethodDecl("Hexagon", "area").
}
