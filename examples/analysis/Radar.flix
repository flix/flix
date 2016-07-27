/**
 * Implementation of the core ideas from the OOPSLA 2015 paper:
 *
 * Static Analysis of Event-Driven Node.js JavaScript Applications
 * - Magnus Madsen, Frank Tip and Ondřej Lhoták
 */

///////////////////////////////////////////////////////////////////////////////
/// Base Analysis                                                           ///
///////////////////////////////////////////////////////////////////////////////

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
    case EventSensitive(Set[(Object, Event)]),
    case ListenerSensitive(Set[(Object, Event, Object)])
}

/**
 * Input Relations.
 */
// Object creation expression: s: v1 = new Object().
rel NewObj(s: Stm, x: Variable, o: Object)

// Assignment expression: s: v1 = v2.
rel Assign(s: Stm, v1: Variable, v2: Variable)

// Field load expression: s: v1 = v2.f
rel Load(s: Stm, v1: Variable, v2: Variable, f: Field)

// Field store expression: s: v1.f = v2
rel Store(s: Stm, v1: Variable, f: Field, v2: Variable)

// Call expressions: s: v1(...).
rel Call(s: Stm, x: Variable)

// Argument (as seen from the caller).
rel CallActual(s: Stm, x: Variable)

// Argument (as seen from the callee).
rel CallFormal(s: Stm, m: Object, x: Variable)

// Return (as seen from the callee).
rel CallReturn(s: Stm, x: Variable)

// Return (as seen from the caller).
rel CallResult(s: Stm, x: Variable)

// Control-flow Graph edge s1 -> s2.
rel CFG(s1: Stm, s2: Stm)


/**
 * Analysis Relations.
 */
// Variable `v` points-to object `o` at statement `s`.
rel VarPointsTo(io: S, s: Stm, x: Variable, o: Object)

// Field `f` of object `o1` points-to object `o2` at statement `s`.
rel HeapPointsTo(io: S, s: Stm, o1: Object, f: Field, o2: Object)

// Call at statement `s` to function object `m`.
rel CallGraph(s: Stm, m: Object)

def isIn(s: S): Bool = match s with {
    case S.In => true
    case _    => false
}

/**
 * Analysis Rules.
 */
// NewObj Rule: s: v = new Object()
VarPointsTo(S.Out, s, v, o) :- NewObj(s, v, o).

// Assign Rule: s: v1 = v2
VarPointsTo(S.Out, s, v1, o) :- isIn(pp), Assign(s, v1, v2), VarPointsTo(pp, s, v2, o).

// Load rule: v1 = v2.f
VarPointsTo(S.Out, s, v1, o2) :- isIn(pp1), isIn(pp2),
                                 Load(s, v1, v2, f),
                                 VarPointsTo(pp1, s, v2, o1),
                                 HeapPointsTo(pp2, s, o1, f, o2).

// Store rule: v1.f = v2
HeapPointsTo(S.Out, s, o1, f, o2) :- isIn(pp1), isIn(pp2),
                                     Store(s, v1, f, v2),
                                     VarPointsTo(pp1, s, v1, o1),
                                     VarPointsTo(pp2, s, v2, o2).

///////////////////////////////////////////////////////////////////////////////
/// Promises                                                                ///
///////////////////////////////////////////////////////////////////////////////

rel Promisify(s: Stm, x: Variable)



//// Call graph.
//CallGraph(s, m) :- Call(s, v),
//                   VarPointsTo(S.In, s, v, m).

//// Call Actual(s1) --> Formal(s2)
//VarPointsTo(S.In, s2, v2, o) :- CallGraph(s1, m),  // TODO: Make sure calls are matched.
//                                CallActual(s1, v1),
//                                VarPointsTo(S.In, s1, v1, o),
//                                CallFormal(s2, m, v2).
//
//// Call Return(s1) --> Call Result(s2)
//VarPointsTo(S.Out, s2, v2, o) :- CallGraph(s2, m), // TODO: Make sure calls are matched.
//                                 CallResult(s2, v2),
//                                 CallReturn(s1, v1),
//                                 VarPointsTo(S.In, s1, v1, o).
//
//// Data flow along the control-flow graph.
//VarPointsTo(S.In, s2, v, o) :- CFG(s1, s2), VarPointsTo(S.Out, s1, v, o).
//VarPointsTo(S.In, s2, v, o) :- CFG(s1, s2), VarPointsTo(S.In, s1, v, o).
//
//HeapPointsTo(S.In, s2, o1, f, o2) :- CFG(s1, s2), HeapPointsTo(S.Out, s1, o1, f, o2).
//HeapPointsTo(S.In, s2, o1, f, o2) :- CFG(s1, s2), HeapPointsTo(S.In, s1, o1, f, o2).
//
//
/////////////////////////////////////////////////////////////////////////////////
///// Events                                                                  ///
/////////////////////////////////////////////////////////////////////////////////
//
///**
// * Input relations.
// */
//
//// Listen expression v1.listen(e, v2).
//rel Listen(s: Stm, v1: Variable, e: Event, v2: Variable)
//
//// Emit expression v1.emit(e, v2).
//rel Emit(s: Stm, v1: Variable, e: Event, v2: Variable)
//
///**
// * Derived relations.
// */
//
//// Listener `l` registered on object `o` for event `e` at statement `s`.
//rel Listener(io: S, s: Stm, o: Object, e: Event, l: Object)
//
//// Listener `l` is enqueue on object `o` for event `e` at statement `s`.
//rel Queue(io: S, s: Stm, o: Object, e: Event, l: Object) // TODO: Need to model argument too..
//
//
//// Listen rule: v1.listen(event, v2).
//Listener(S.Out, s, o, e, l) :- Listen(s, v1, e, v2),
//                               VarPointsTo(S.In, s, v1, o),
//                               VarPointsTo(S.In, s, v2, l).
//
//// Emit rule: v1.emit(event, v2).
//Queue(S.Out, s, o, e, l) :- Emit(v1, e, _),
//                            VarPointsTo(S.In, s, v1, o),
//                            Listener(S.In, s, o, e, l).
//
//// TODO: 1. Call event listeners in the queue. (modelling the event loop)
//// TODO: 2. New to add edge to CFG s.t. the main function enters the event loop when complete.
//         // or with similar means.
//         // If the last statement in main is sX
//         // then I would add the fact:
//         // CFG(sX, "EventLoop").
//
//// TODO: 3. Implement the context sensitive policies.
//
//// TODO: Flow sensitivity and strong updates.
//// TODO: Replace 'f' by 'p' and 'm' by 'f'.
//
