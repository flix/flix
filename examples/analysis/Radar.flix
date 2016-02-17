/**
 * Implementation of the core ideas from the OOPSLA 2015 paper:
 *
 * Static Analysis of Event-Driven Node.js JavaScript Applications
 * - Magnus Madsen, Frank Tip and Ondřej Lhoták
 */

/**
 * Input types.
 */
enum Object { case Object(Str) }
enum Event { case Event(Str) }
enum Field { case Field(Str) }
enum Variable { case Var(Str) }
enum Stm { case Stm(Str) }

/**
 * Input Relations.
 */
// Object creation expression: s: v1 = new Object().
rel NewObj(s: Stm, v: Variable, o: Object)

// Assignment expression: s: v1 = v2.
rel Assign(s: Stm, v1: Variable, v2: Variable)

// Field load expression: s: v1 = v2.f
rel Load(s: Stm, v1: Variable, v2: Variable, f: Field)

// Field store expression: s: v1.f = v2
rel Store(s: Stm, v1: Variable, f: Field, v2: Variable)

// Call expressions: s: v1(v2).
rel Call(s: Stm, v1: Variable, v2: Variable)

// Argument (as seen from the callee).
rel MethodFormal(s: Stm, m: Object, v: Variable)

// Return (as seen from the callee).
rel MethodReturn(s: Stm, m: Object, v: Variable)

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

// The variable points-to graph immediately before statement `s`.
rel VarPointsTo(s: Stm, v: Variable, o: Object)

// The heap points-to graph immediately before statement `s`.


// NewObject rule.
VarPointsTo(s2, v, o) :- CFG(s1, s2), NewObj(s1, v, o).

// Assign rule.
VarPointsTo(s2, v2, o) :- CFG(s1, s2), Assign(v1, v2), VarPointsTo(v1, o).



//rel HeapPointsTo()

// rel CallActual (as seen from the call site).
// rel CallReturn (as seen from the clal site).