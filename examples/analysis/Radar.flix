/**
 * Implementation of the core ideas from the OOPSLA 2015 paper:
 *
 * Static Analysis of Event-Driven Node.js JavaScript Applications
 * - Magnus Madsen, Frank Tip and Ondřej Lhoták
 */

/**
 * Input types.
 */
enum AllocSite { case AllocSite(Str) }
enum Field { case Field(Str) }
enum Var { case Var(Str) }

/**
 * Input Relations.
 */
// Object creation expression: v1 = new Object().
rel NewObj(v1: Variable, o: AllocSite)

// Assignment expression: v1 = v2.
rel Assign(v1: Variable, v2: Variable)



/**
 * Derived Relations.
 */
enum Context {
    case None,
    case Listener(Set[Str]),
    case Events(Set[Str])
}

rel VarPointsTo(v: )

rel HeapPointsTo()
