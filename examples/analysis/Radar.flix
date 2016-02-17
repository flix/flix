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
enum Stm { case In(Str), case Out(Str) }

enum Context {
    case None,
    case Listener(Set[Str]),
    case Events(Set[Str])
}

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

// Argument (as seen from the caller).
rel CallActual(s: Stm, v: Variable)

// Argument (as seen from the callee).
rel CallFormal(s: Stm, m: Object, v: Variable)

// Return (as seen from the callee).
rel CallReturn(s: Stm, v: Variable)

// Return (as seen from the caller).
rel CallResult(s: Stm, v: Variable)

// Control-flow Graph edge s1 -> s2.
rel CFG(s1: Stm, s2: Stm)


/**
 * Analysis Relations.
 */
// The variable points-to graph immediately before statement `s`.
rel VarPointsTo(s: Stm, v: Variable, o: Object)

// The heap points-to graph immediately before statement `s`.
rel HeapPointsTo(s: Stm, o1: Object, f: Field, o2: Object)

/**
 * Analysis Rules.
 */
// NewObject rule.
VarPointsTo(s2, v, o) :- CFG(s1, s2), NewObj(s1, v, o).

// Assign rule.
VarPointsTo(s2, v2, o) :- CFG(s1, s2), Assign(v1, v2), VarPointsTo(v1, o).

// Load rule.
VarPointsTo(s2, v1, o2) :- Load(s1, v1, v2, f),
                           VarPointsTo(s1, v2, o1)
                           HeapPointsTo(s1, o1, f, o2).

// Store rule.
HeapPointsTo(s2, o1, f, o2) :- Store(s1, v1, f, v2),
                               VarPointsTo(s1, v1, o1),
                               VarPointsTo(s1, v2, o2).

// Call Actual --> Formal


// Call Return --> Call Result


// Data flow along the control-flow graph.
// HeapPointsTo(s2, o1, f, o2) :- CFG(s1, s2), HeapPointsTo()


/**
 * Events
 */

// Events
// Listener: Addr x Event -> Lam*
// Queue: Addr x Event -> (Lam x Val)*

// Event sensitive (Addr x Event) Set
// Listener sensitive (Addr x event x Lambda) Set
