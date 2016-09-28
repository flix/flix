///////////////////////////////////////////////////////////////////////////////
// Common Data Types                                                         //
///////////////////////////////////////////////////////////////////////////////

/**
 * Input types.
 */
enum Obj { case Obj(Str) }
enum Field { case Field(Str) }
enum Var { case Var(Str) }
enum Stm { case Stm(Str)}

enum Ctx {
    case None
}

enum State {
    case Pending,
    case Fulfilled,
    case Rejected
}


///////////////////////////////////////////////////////////////////////////////
// Declarations: Input Core Expressions                                      //
///////////////////////////////////////////////////////////////////////////////

// Obj creation expression: s: v = new Obj().
rel NewObj(s: Stm, v: Var, o: Obj)

// Assignment expression: s: v1 = v2.
rel Assign(s: Stm, v1: Var, v2: Var)

// Field load expression: s: v1 = v2.f
rel Load(s: Stm, v1: Var, v2: Var, f: Field)

// Field store expression: s: v1.f = v2
rel Store(s: Stm, v1: Var, f: Field, v2: Var)

// Call expressions: s: v1(v2).
rel Call(s: Stm, v1: Var, v2: Var)

// Argument (as seen from the caller).
rel CallActual(s: Stm, v: Var)

// Argument (as seen from the callee).
rel CallFormal(s: Stm, m: Obj, v: Var)

// Return (as seen from the callee).
rel CallReturn(s: Stm, v: Var)

// Return (as seen from the caller).
rel CallResult(s: Stm, v: Var)

// Control-flow Graph edge s1 -> s2.
rel CFG(s1: Stm, s2: Stm)


///////////////////////////////////////////////////////////////////////////////
// Declarations: Input Promise Expressions                                   //
///////////////////////////////////////////////////////////////////////////////

// Initialize promise expression: s: promisify(v)
rel Promisify(s: Stm, v: Var)

// Resolve promise expression: s: v1.resolve(v2)
rel Resolve(s: Stm, v1: Var, v2: Var)

// Reject promise expression: s: v1.reject(v2)
rel Reject(s: Stm, v1: Var, v2: Var)

// On resolve expression: s: v1.onResolve(v2)
rel OnResolve(s: Stm, v1: Var, v2: Var)

// On reject expression: s: v1.onReject(v2)
rel OnReject(s: Stm, v1: Var, v2: Var)

// Promise link expression: s: v1.link(v2)
rel Link(s: Stm, v1: Var, v2: Var)

///////////////////////////////////////////////////////////////////////////////
// Declarations: Relations derived by Analysis                               //
///////////////////////////////////////////////////////////////////////////////

 // The statement `s` is reachable in context `c`.
rel Reachable(c: Ctx, s: Stm)

// Call at statement `s` to function object `m`.
rel CallGraph(s: Stm, m: Obj)

// Var `v` points-to object `o` at statement `s` in context `c`.
rel VarPointsToIn(c: Ctx, s: Stm, x: Var, o: Obj)
rel VarPointsToOut(c: Ctx, s: Stm, x: Var, o: Obj)

// Field `f` of object `o1` points-to object `o2` at statement `s` in context `c`.
rel HeapPointsToIn(c: Ctx, s: Stm, o1: Obj, f: Field, o2: Obj)
rel HeapPointsToOut(c: Ctx, s: Stm, o1: Obj, f: Field, o2: Obj)

// The promise `o` is in promise state `t` with value `v` at statement `s` in context `c`.
// If the promise is pending then the value is the special `null` object.
rel PromiseStateIn(c: Ctx, s: Stm, o: Obj, t: State, v: Obj)
rel PromiseStateOut(c: Ctx, s: Stm, o: Obj, t: State, v: Obj)

// The promise `o` has `r` registered as a fulfill reaction at statement `s` in context `c`.
rel FulfillReactionIn(c: Ctx, s: Stm, o: Obj, r: Obj)
rel FulfillReactionOut(c: Ctx, s: Stm, o: Obj, r: Obj)

// The promise `o` has `r` registered as a reject reaction at statement `s` in context `c`.
rel RejectReactionIn(c: Ctx, s: Stm, o: Obj, r: Obj)
rel RejectReactionOut(c: Ctx, s: Stm, o: Obj, r: Obj)

// The promise `o1` is linked to `o2` at statement `s` in context `c`.
// If `o1` is linked to `o2` then when `o1` is resolved/rejected so is `o2`.
rel PromiseLinkIn(c: Ctx, s: Stm, o1: Obj, o2: Obj)
rel PromiseLinkOut(c: Ctx, s: Stm, o1: Obj, o2: Obj)

// TODO: Queue

///////////////////////////////////////////////////////////////////////////////
// Declarations: Promise Graph                                               //
///////////////////////////////////////////////////////////////////////////////

// A resolve edge o -> p from a value `o` to a promise `p`.
rel ResolveEdge(o: Obj, p: Obj)

// A reject edge o -> p from a value `o` to a promise `p`.
rel RejectEdge(o: Obj, p: Obj)

// An onResolve edge p -> m from a promise `o` to a function `m`.
rel OnResolveEdge(o: Obj, m: Obj)

// An onReject edge p -> m from a promise `o` to a function `m`.
rel OnRejectEdge(p: Obj, m: Obj)

// A promise link edge p1 -> p2 from one promise `p1` to another promise `p2`.
rel LinkEdge(p1: Obj, p2: Obj)


///////////////////////////////////////////////////////////////////////////////
// Semantics: Core Expressions                                               //
///////////////////////////////////////////////////////////////////////////////

// NewObj Rule: s: v = new Obj()
VarPointsToOut(c, s, v, o) :- NewObj(s, v, o),
                              Reachable(c, s).

// Assign Rule: s: v1 = v2
VarPointsToOut(c, s, v1, o) :- Assign(s, v1, v2),
                               VarPointsToIn(c, s, v2, o),
                               Reachable(c, s).

// Load rule: v1 = v2.f
VarPointsToOut(c, s, v1, o2) :- Load(s, v1, v2, f),
                                VarPointsToIn(c, s, v2, o1),
                                HeapPointsToIn(c, s, o1, f, o2),
                                Reachable(c, s).

// Store rule: v1.f = v2
HeapPointsToOut(c, s, o1, f, o2) :- Store(s, v1, f, v2),
                                    VarPointsToIn(c, s, v1, o1),
                                    VarPointsToIn(c, s, v2, o2),
                                    Reachable(c, s).

///////////////////////////////////////////////////////////////////////////////
// Semantics: Promise Expressions                                            //
///////////////////////////////////////////////////////////////////////////////

// Mark the entry point as reachable.
Reachable(Ctx.None, Stm.Stm("l1")).

// Propagate reachability along the control-flow graph.
Reachable(c, s2) :- CFG(s1, s2), Reachable(c, s1).

// Propagate dataflow along the control-flow graph.
VarPointsToIn(c, s2, v, o) :- CFG(s1, s2), VarPointsToOut(c, s1, v, o).
VarPointsToOut(c, s, v, o) :- VarPointsToIn(c, s, v, o).

HeapPointsToIn(c, s2, o1, f, o2) :- CFG(s1, s2), HeapPointsToOut(c, s1, o1, f, o2).
HeapPointsToOut(c, s, o1, f, o2) :- HeapPointsToIn(c, s, o1, f, o2).

///////////////////////////////////////////////////////////////////////////////
// Semantics: Promise Graph                                                  //
///////////////////////////////////////////////////////////////////////////////





///////////////////////////////////////////////////////////////////////////////
// Example Program                                                           //
///////////////////////////////////////////////////////////////////////////////

// Input program
// var o1 = {};
// var o2 = {};
// o2.f = o1
NewObj(Stm.Stm("l1"), Var.Var("o1"), Obj.Obj("site1")).
NewObj(Stm.Stm("l2"), Var.Var("o2"), Obj.Obj("site2")).
Store(Stm.Stm("l3"), Var.Var("o2"), Field.Field("f"), Var.Var("o1")).

CFG(Stm.Stm("l1"), Stm.Stm("l2")).
CFG(Stm.Stm("l2"), Stm.Stm("l3")).