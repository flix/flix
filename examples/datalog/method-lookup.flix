///
/// We introduce type aliases to model the class and method domain.
///
type alias Class = String       // e.g. "Rectangle"
type alias Interface = String   // e.g. "Shape"
type alias Method = String      // e.g. "draw"

///
/// We compute all methods available on a given type and use provenance
/// queries to determine which class each method is declared in.
///
pub def main(): Unit \ IO =
    let targetType = "Square";
    let db = database();

    // A Datalog program that computes the transitive supertype relation
    // and available methods (including inherited ones).
    let pr = #{
        // A supertype relationship can come from class extension or interface implementation.
        SuperType(x, y) :- Extends(x, y).
        SuperType(x, y) :- Implements(x, y).

        // Compute transitive closure of supertype relationships.
        SuperType(x, z) :- SuperType(x, y), Extends(y, z).
        SuperType(x, z) :- SuperType(x, y), Implements(y, z).

        // A method is available on a type if it's declared directly on that type.
        AvailableMethod(typ, method) :- MethodDecl(typ, method).

        // A method is also available if it's declared on a supertype.
        AvailableMethod(typ, method) :- SuperType(typ, superTyp), MethodDecl(superTyp, method).
    };

    // Query for all methods available on the target type.
    let methods = query db, pr select method from AvailableMethod(targetType, method);

    if (Vector.isEmpty(methods)) {
        println("No methods found on type '${targetType}'.")
    } else {
        println("Methods available on '${targetType}':\n");

        foreach (method <- methods) {
            // Use a provenance query to find where this method is declared.
            let p = pquery db, pr select AvailableMethod(targetType, method) with {MethodDecl};

            // Extract the declaring class from the provenance.
            foreach (decl <- p) {
                ematch decl {
                    case MethodDecl(declaringClass, _) => {
                        let fmtMethod = String.padRight(15, ' ', method);
                        let source = if (declaringClass == targetType) "self" else "inherited";
                        let fmtSource = String.padRight(10, ' ', source);
                        println("  ${fmtMethod} <- ${declaringClass} (${fmtSource})")
                    }
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
