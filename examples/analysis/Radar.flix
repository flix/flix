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
enum Event { case Event(Str) }
enum Field { case Field(Str) }
enum Variable { case Var(Str) }
enum Stm { case Stm(Str) }

/**
 * Input Relations.
 */
// Object creation expression: s: v1 = new Object().
rel NewObj(s: Stm, v: Variable, o: AllocSite)

// Assignment expression: s: v1 = v2.
rel Assign(s: Stm, v1: Variable, v2: Variable)

// Field load expression: s: v1 = v2.f
rel Load(s: Stm, v1: Variable, v2: Variable, f: Field)

// Field store expression: s: v1.f = v2
rel Store(s: Stm, v1: Variable, f: Field, v2: Variable)

// Call expressions: s: v1(v2).
rel Call(s: Stm, v1: Variable, v2: Variable)

// Argument (as seen from the callee).
rel MethodFormal(s: Stm, m: AllocSite, v: Variable)

// Return (as seen from the callee).
rel MethodReturn(s: Stm, m: AllocSite, v: Variable)

// Control-flow Graph edge s1 -> s2.
rel CFG(s1: Stm, s2: Stm)

// Events


/**
 * Derived Relations.
 */
enum Context {
    case None,
    case Listener(Set[Str]),
    case Events(Set[Str])
}

//rel VarPointsTo(v: )

//rel HeapPointsTo()

// rel CallActual (as seen from the call site).
// rel CallReturn (as seen from the clal site).