///////////////////////////////////////////////////////////////////////////////
// Analysis Types and Relations                                              //
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

/**
 * Input Relations.
 */
// Obj creation expression: s: v1 = new Obj().
rel NewObj(s: Stm, x: Var, o: Obj)

// Assignment expression: s: v1 = v2.
rel Assign(s: Stm, v1: Var, v2: Var)

// Field load expression: s: v1 = v2.f
rel Load(s: Stm, v1: Var, v2: Var, f: Field)

// Field store expression: s: v1.f = v2
rel Store(s: Stm, v1: Var, f: Field, v2: Var)

// Call expressions: s: x(...).
rel Call(s: Stm, x: Var)

// Argument (as seen from the caller).
rel CallActual(s: Stm, x: Var)

// Argument (as seen from the callee).
rel CallFormal(s: Stm, m: Obj, x: Var)

// Return (as seen from the callee).
rel CallReturn(s: Stm, x: Var)

// Return (as seen from the caller).
rel CallResult(s: Stm, x: Var)

// Control-flow Graph edge s1 -> s2.
rel CFG(s1: Stm, s2: Stm)



/**
 * Analysis Relations.
 */
 // The statement `s` is reachable in context `c`.
rel Reachable(c: Ctx, s: Stm)

// Var `v` points-to object `o` at statement `s` in context `c`.
rel VarPointsToIn(c: Ctx, s: Stm, x: Var, o: Obj)
rel VarPointsToOut(c: Ctx, s: Stm, x: Var, o: Obj)

// Field `f` of object `o1` points-to object `o2` at statement `s` in context `c`..
rel HeapPointsToIn(c: Ctx, s: Stm, o1: Obj, f: Field, o2: Obj)
rel HeapPointsToOut(c: Ctx, s: Stm, o1: Obj, f: Field, o2: Obj)

// Call at statement `s` to function object `m`.
rel CallGraph(s: Stm, m: Obj)

///////////////////////////////////////////////////////////////////////////////
// Abstract Semantics                                                        //
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
// Reachability                                                              //
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