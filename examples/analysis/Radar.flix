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
enum Stm { case Stm(Str)}
enum S { case In, case Out }

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

// Call expressions: s: v1(...).
rel Call(s: Stm, v1: Variable)

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
rel VarPointsTo(io: S, s: Stm, v: Variable, o: Object)

// The heap points-to graph immediately before statement `s`.
rel HeapPointsTo(s: Stm, o1: Object, f: Field, o2: Object)

// The call graph.
rel CallGraph(s: Stm, m: Object)

/**
 * Analysis Rules.
 */
// NewObject rule.
VarPointsTo(S.Out, s, v, o) :- NewObj(s, v, o).

// Assign rule.
VarPointsTo(S.Out, s, v2, o) :- Assign(v1, v2), VarPointsTo(S.In, s, v1, o).

// Load rule.
VarPointsTo(s2, v1, o2) :- Load(s1, v1, v2, f),
                           VarPointsTo(s1, v2, o1)
                           HeapPointsTo(s1, o1, f, o2).

// Store rule.
HeapPointsTo(s2, o1, f, o2) :- Store(s1, v1, f, v2),
                               VarPointsTo(s1, v1, o1),
                               VarPointsTo(s1, v2, o2).

// Call graph.
CallGraph(s, m) :- Call(s, v),
                   VarPointsTo(s, v, m).

// Call Actual --> Formal
VarPointsTo(s2, v2, o) :- CallGraph(s1, m),
                          CallActual(s1, v1),
                          VarPointsTo(v1, o)
                          CallFormal(s2, m, v2).

// Call Return --> Call Result
VarPointsTo(s2, v2, o) :- CallGraph(s2, m),
                          CallReturn(s1, v1),
                          VarPointsTo(s1, v1, o),
                          CallResult(s2, v2).

// Data flow along the control-flow graph.
VarPointsTo(s2, v, o) :- CFG(s1, s2), VarPointsTo(s1, v, o).
HeapPointsTo(s2, o1, f, o2) :- CFG(s1, s2), HeapPointsTo(s1, o1, f, o2).

/**
 * Events
 */

// Events
// Listener: Addr x Event -> Lam*
// Queue: Addr x Event -> (Lam x Val)*

// Event sensitive (Addr x Event) Set
// Listener sensitive (Addr x event x Lambda) Set

// TODO: Flow sensitivity and strong updates.